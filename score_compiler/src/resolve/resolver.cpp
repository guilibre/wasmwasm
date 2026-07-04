#include "resolver.hpp"

#include <algorithm>
#include <unordered_map>
#include <unordered_set>
#include <variant>

namespace {

using MotivTable = std::unordered_map<std::string, const MotivDef *>;

struct ResolverState {
    int octave = 4;
    std::optional<double> duration;
};

struct Track {
    std::vector<ResolvedStep> steps;
    ResolverState state;
};

auto resolve_seq_expr(const SeqExpr &expr, const ResolverState &initial_state,
                      const MotivTable &motivs,
                      std::unordered_set<std::string> &resolving)
    -> std::vector<Track>;

auto resolve_term(const Term &term, const ResolverState &incoming,
                  const MotivTable &motivs,
                  std::unordered_set<std::string> &resolving)
    -> std::vector<Track>;

void check_references(const SeqExpr &expr,
                      const std::unordered_set<std::string> &defined) {
    for (const auto &term : expr.terms) {
        for (const auto &branch : term.branches) {
            if (const auto *group_ref = std::get_if<GroupRef>(&branch)) {
                check_references(group_ref->group->body, defined);
            } else if (const auto *name = std::get_if<std::string>(&branch)) {
                if (!defined.contains(*name))
                    throw ResolveException(
                        "undefined or forward-referenced motiv '" + *name + "'",
                        term.line, term.column);
            }
        }
    }
}

auto pitch_class_of(char letter, int accidental) -> int {
    int pitch_class = 0;
    switch (letter) {
    case 'C':
        pitch_class = 0;
        break;
    case 'D':
        pitch_class = 2;
        break;
    case 'E':
        pitch_class = 4;
        break;
    case 'F':
        pitch_class = 5;
        break;
    case 'G':
        pitch_class = 7;
        break;
    case 'A':
        pitch_class = 9;
        break;
    case 'B':
        pitch_class = 11;
        break;
    default:
        break;
    }
    return pitch_class + accidental;
}

auto midi_of(int pitch_class, int octave) -> int {
    return ((octave + 1) * 12) + pitch_class;
}

auto resolve_duration(const Duration &duration, ResolverState &state,
                      size_t line, size_t column) -> double {
    if (duration.numerator.has_value()) {
        const double beats =
            static_cast<double>(*duration.numerator) / duration.denominator;
        state.duration = beats;
        return beats;
    }

    if (!state.duration.has_value())
        throw ResolveException(
            "no duration in effect; write an explicit '@duration'", line,
            column);
    return *state.duration;
}

auto resolve_voice(const Voice &voice, ResolverState &state) -> ResolvedNote {
    const double beats = resolve_duration(voice.duration, state,
                                          voice.atom.line, voice.atom.column);

    if (voice.atom.is_rest)
        return ResolvedNote{
            .is_rest = true, .midinote = 0, .duration_beats = beats};

    int octave = 0;
    if (voice.atom.octave.has_value()) {
        octave = *voice.atom.octave;
        state.octave = octave;
    } else {
        octave = state.octave;
    }

    const int midinote = midi_of(
        pitch_class_of(voice.atom.letter, voice.atom.accidental), octave);
    return ResolvedNote{
        .is_rest = false, .midinote = midinote, .duration_beats = beats};
}

auto resolve_group_duration(const Duration &duration,
                            const std::vector<ResolvedStep> &steps) -> double {
    if (duration.numerator.has_value())
        return static_cast<double>(*duration.numerator) / duration.denominator;

    double advance = 0;
    for (const ResolvedStep &step : steps)
        advance = std::max(advance, step.wait_beats);
    return advance;
}

auto resolve_term(const Term &term, const ResolverState &incoming,
                  const MotivTable &motivs,
                  std::unordered_set<std::string> &resolving)
    -> std::vector<Track> {
    const bool all_voices =
        std::ranges::all_of(term.branches, [](const Branch &b) -> bool {
            return std::holds_alternative<Voice>(b);
        });

    if (all_voices) {
        if (term.branches.size() > 1)
            for (const Branch &branch : term.branches)
                if (std::get<Voice>(branch).atom.is_rest)
                    throw ResolveException(
                        "a rest cannot be part of a '|' chord", term.line,
                        term.column);

        ResolverState state = incoming;
        std::vector<ResolvedNote> notes;
        double wait_beats = 0;
        for (const Branch &branch : term.branches) {
            ResolvedNote note = resolve_voice(std::get<Voice>(branch), state);
            wait_beats = std::max(wait_beats, note.duration_beats);
            notes.push_back(note);
        }

        Track track;
        track.steps.push_back(
            ResolvedStep{.notes = std::move(notes), .wait_beats = wait_beats});
        track.state = state;

        std::vector<Track> result;
        result.push_back(std::move(track));
        return result;
    }

    std::vector<Track> result;
    for (const Branch &branch : term.branches) {
        if (const auto *voice = std::get_if<Voice>(&branch)) {
            ResolverState state = incoming;
            ResolvedNote note = resolve_voice(*voice, state);
            Track track;
            track.steps.push_back(ResolvedStep{
                .notes = {note}, .wait_beats = note.duration_beats});
            track.state = state;
            result.push_back(std::move(track));
            continue;
        }

        if (const auto *group_ref = std::get_if<GroupRef>(&branch)) {
            auto nested = resolve_seq_expr(group_ref->group->body, incoming,
                                           motivs, resolving);
            for (Track &nested_track : nested) {
                const double advance = resolve_group_duration(
                    group_ref->duration, nested_track.steps);
                nested_track.steps.back().wait_beats = advance;
                result.push_back(std::move(nested_track));
            }
            continue;
        }

        const auto &name = std::get<std::string>(branch);
        auto it = motivs.find(name);
        if (it == motivs.end())
            throw ResolveException("undefined motiv '" + name + "'", term.line,
                                   term.column);
        if (resolving.contains(name))
            throw ResolveException("motiv '" + name +
                                       "' references itself (directly or "
                                       "indirectly)",
                                   term.line, term.column);

        resolving.insert(name);
        ResolverState fresh_state;
        auto nested =
            resolve_seq_expr(it->second->body, fresh_state, motivs, resolving);
        resolving.erase(name);

        for (Track &nested_track : nested)
            result.push_back(std::move(nested_track));
    }
    return result;
}

auto resolve_seq_expr(const SeqExpr &expr, const ResolverState &initial_state,
                      const MotivTable &motivs,
                      std::unordered_set<std::string> &resolving)
    -> std::vector<Track> {
    std::vector<Track> tracks;
    tracks.push_back(Track{.steps = {}, .state = initial_state});

    for (const Term &term : expr.terms) {
        std::vector<Track> next_tracks;
        for (const Track &track : tracks) {
            auto term_tracks =
                resolve_term(term, track.state, motivs, resolving);
            for (Track &term_track : term_tracks) {
                Track combined;
                combined.steps = track.steps;
                combined.steps.insert(
                    combined.steps.end(),
                    std::make_move_iterator(term_track.steps.begin()),
                    std::make_move_iterator(term_track.steps.end()));
                combined.state = term_track.state;
                next_tracks.push_back(std::move(combined));
            }
        }
        tracks = std::move(next_tracks);
    }

    return tracks;
}

} // namespace

auto resolve_program(const Program &program) -> ResolvedProgram {
    std::unordered_set<std::string> defined;
    MotivTable motivs;

    for (const MotivDef &motiv : program.motivs) {
        check_references(motiv.body, defined);
        if (motivs.contains(motiv.name))
            throw ResolveException(
                "motiv '" + motiv.name + "' is already defined", 0, 0);
        defined.insert(motiv.name);
        motivs.emplace(motiv.name, &motiv);
    }

    ResolvedProgram result;
    for (const PlayTarget &target : program.play.targets) {
        auto it = motivs.find(target.name);
        if (it == motivs.end())
            throw ResolveException("undefined motiv '" + target.name + "'",
                                   target.line, target.column);

        std::unordered_set<std::string> resolving{target.name};
        ResolverState state;
        auto tracks =
            resolve_seq_expr(it->second->body, state, motivs, resolving);
        for (Track &track : tracks) {
            ResolvedSequence sequence;
            sequence.steps = std::move(track.steps);
            result.tracks.push_back(std::move(sequence));
        }
    }
    return result;
}
