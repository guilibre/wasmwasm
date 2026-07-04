#include "parser.hpp"

#include <cctype>

Parser::Parser(Tokenizer tokenizer)
    : tokenizer(tokenizer), current(this->tokenizer.next()) {}

void Parser::advance() { current = tokenizer.next(); }

auto Parser::match(TokenKind kind) const -> bool {
    return current.kind == kind;
}

auto Parser::expect(TokenKind kind, const std::string &what) -> Token {
    if (match(TokenKind::Invalid))
        throw ParseException(current.lexeme, current.line, current.column);

    if (!match(kind))
        throw ParseException("Expected " + what + " but got '" +
                                 current.lexeme + "'",
                             current.line, current.column);

    auto token = current;
    advance();
    return token;
}

void Parser::skip_newlines() {
    while (match(TokenKind::Newline)) advance();
}

void Parser::end_statement() {
    if (match(TokenKind::Eof)) return;
    expect(TokenKind::Newline, "end of line");
}

auto Parser::starts_term() const -> bool {
    return match(TokenKind::Note) || match(TokenKind::Rest) ||
           match(TokenKind::LParen) || match(TokenKind::Ident);
}

auto Parser::parse() -> Program {
    Program program;
    std::optional<PlayStmt> play;

    skip_newlines();
    while (!match(TokenKind::Eof)) {
        if (match(TokenKind::Play)) {
            if (play.has_value())
                throw ParseException("only one 'play' statement is allowed",
                                     current.line, current.column);
            play = parse_play_stmt();
        } else if (match(TokenKind::Ident)) {
            program.motivs.push_back(parse_motiv_def());
        } else {
            throw ParseException("Expected a motiv definition or 'play' "
                                 "statement but got '" +
                                     current.lexeme + "'",
                                 current.line, current.column);
        }
        skip_newlines();
    }

    if (!play.has_value())
        throw ParseException("expected a 'play' statement", current.line,
                             current.column);

    program.play = *play;
    return program;
}

auto Parser::parse_motiv_def() -> MotivDef {
    auto name = expect(TokenKind::Ident, "a motiv name");
    expect(TokenKind::Equals, "'='");
    MotivDef motiv;
    motiv.name = name.lexeme;
    motiv.body = parse_seq_expr();
    end_statement();
    return motiv;
}

auto Parser::parse_play_stmt() -> PlayStmt {
    expect(TokenKind::Play, "'play'");

    PlayStmt stmt;
    auto name = expect(TokenKind::Ident, "a motiv name");
    stmt.targets.push_back(PlayTarget{
        .name = name.lexeme, .line = name.line, .column = name.column});

    while (match(TokenKind::Pipe)) {
        advance();
        auto next_name = expect(TokenKind::Ident, "a motiv name");
        stmt.targets.push_back(PlayTarget{.name = next_name.lexeme,
                                          .line = next_name.line,
                                          .column = next_name.column});
    }

    end_statement();
    return stmt;
}

auto Parser::parse_seq_expr() -> SeqExpr {
    SeqExpr expr;
    if (!starts_term())
        throw ParseException("expected at least one note, rest, group or "
                             "motiv reference",
                             current.line, current.column);
    while (starts_term()) expr.terms.push_back(parse_term());
    return expr;
}

auto Parser::parse_term() -> Term {
    Term term;
    term.line = current.line;
    term.column = current.column;

    term.branches.push_back(parse_branch());
    while (match(TokenKind::Pipe)) {
        advance();
        term.branches.push_back(parse_branch());
    }
    return term;
}

auto Parser::parse_branch() -> Branch {
    if (match(TokenKind::LParen)) {
        auto group = parse_group();
        Duration duration;
        if (match(TokenKind::At)) {
            advance();
            duration = parse_duration();
        }
        return GroupRef{.group = std::move(group), .duration = duration};
    }

    if (match(TokenKind::Ident)) {
        auto name = expect(TokenKind::Ident, "a motiv name");
        return name.lexeme;
    }

    return parse_voice();
}

auto Parser::parse_group() -> std::unique_ptr<Group> {
    expect(TokenKind::LParen, "'('");
    auto group = std::make_unique<Group>();
    group->body = parse_seq_expr();
    expect(TokenKind::RParen, "')'");
    return group;
}

auto Parser::parse_voice() -> Voice {
    Voice voice;
    voice.atom = parse_note_atom();
    if (match(TokenKind::At)) {
        advance();
        voice.duration = parse_duration();
    }
    return voice;
}

auto Parser::parse_note_atom() -> NoteAtom {
    NoteAtom atom;
    atom.line = current.line;
    atom.column = current.column;

    if (match(TokenKind::Rest)) {
        auto token = expect(TokenKind::Rest, "a rest");
        atom.is_rest = true;
        return atom;
    }

    auto token = expect(TokenKind::Note, "a note or rest");
    const auto &lexeme = token.lexeme;
    atom.letter =
        static_cast<char>(std::toupper(static_cast<unsigned char>(lexeme[0])));

    size_t pos = 1;
    if (pos < lexeme.size() && (lexeme[pos] == '#' || lexeme[pos] == 'b')) {
        atom.accidental = lexeme[pos] == '#' ? 1 : -1;
        pos++;
    }

    if (pos < lexeme.size()) atom.octave = std::stoi(lexeme.substr(pos));

    return atom;
}

auto Parser::parse_duration() -> Duration {
    auto numerator = expect(TokenKind::Number, "a duration number");
    Duration duration{
        .numerator = std::stoi(numerator.lexeme),
        .denominator = 1,
    };

    if (match(TokenKind::Slash)) {
        advance();
        auto denominator = expect(TokenKind::Number, "a denominator number");
        duration.denominator = std::stoi(denominator.lexeme);
    }

    return duration;
}
