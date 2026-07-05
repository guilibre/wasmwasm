#include "codegen.hpp"

void CodeGenerator::generate(const ResolvedProgram &program,
                             std::ostream &out) {
    generate_preamble(out);
    generate_constructor(out);
    generate_run(program, out);
    for (size_t i = 0; i < program.tracks.size(); ++i)
        generate_track(i, program.tracks[i], out);
    out << "}\n";
}

void CodeGenerator::generate_preamble(std::ostream &out) {
    out << "export interface Event {\n"
        << "    readonly midinote: number;\n"
        << "    readonly velocity: number;\n"
        << "    readonly duration: number; // beats\n"
        << "}\n"
        << "\n"
        << "export interface Runtime {\n"
        << "    emit(event: Event): Promise<void>;\n"
        << "    wait(beats: number): Promise<void>;\n"
        << "    spawn?(fn: () => Promise<void>): void;\n"
        << "}\n"
        << "\n"
        << "export class Score {\n";
}

void CodeGenerator::generate_constructor(std::ostream &out) {
    out << "    constructor(private readonly runtime: Runtime) {}\n";
}

void CodeGenerator::generate_run(const ResolvedProgram &program,
                                 std::ostream &out) {
    out << "\n"
        << "    public async run(): Promise<void> {\n"
        << "        const tracks: Array<() => Promise<void>> = [\n";
    for (size_t i = 0; i < program.tracks.size(); ++i)
        out << "            this.track" << i << ".bind(this),\n";
    out << "        ];\n"
        << "        if (this.runtime.spawn) {\n"
        << "            for (const track of tracks) "
           "this.runtime.spawn(track);\n"
        << "            return;\n"
        << "        }\n"
        << "        await Promise.all(tracks.map((track) => track()));\n"
        << "    }\n";
}

void CodeGenerator::generate_track(size_t index,
                                   const ResolvedSequence &sequence,
                                   std::ostream &out) {
    out << "\n"
        << "    private async track" << index << "(): Promise<void> {\n";
    for (const ResolvedStep &step : sequence.steps) generate_step(step, out);
    out << "    }\n";
}

void CodeGenerator::generate_step(const ResolvedStep &step, std::ostream &out) {
    for (const ResolvedNote &note : step.notes) {
        if (note.is_rest) continue;
        out << "      await this.runtime.emit({ midinote: " << note.midinote
            << ", velocity: 100, duration: " << note.duration_beats << " });\n";
    }
}
