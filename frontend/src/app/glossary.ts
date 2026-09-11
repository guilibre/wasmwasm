import type { StringKey } from '../i18n/strings';

interface GlossaryTerm {
    pattern: RegExp;
    definition_key: StringKey;
}

const GLOSSARY_TERMS: GlossaryTerm[] = [
    { pattern: /\bconst\b/i, definition_key: 'glossary_term_const' },
    { pattern: /\bscale\b/i, definition_key: 'glossary_term_scale' },
    { pattern: /undefined variable/i, definition_key: 'glossary_term_undefined_variable' },
    { pattern: /circular reference/i, definition_key: 'glossary_term_circular_reference' },
    { pattern: /\brepeat\b/i, definition_key: 'glossary_term_repeat' },
    { pattern: /\breverse\b/i, definition_key: 'glossary_term_reverse' },
    { pattern: /\bskip\b/i, definition_key: 'glossary_term_skip' },
    { pattern: /pipe operator|\|>/, definition_key: 'glossary_term_pipe' },
    { pattern: /forks?\s*\(&\)|\bfork\b/i, definition_key: 'glossary_term_fork' },
    { pattern: /'~'|legato/i, definition_key: 'glossary_term_legato' },
    { pattern: /\bsequence\b/i, definition_key: 'glossary_term_sequence' },
    { pattern: /\batomic\b/i, definition_key: 'glossary_term_atomic' },
    { pattern: /@\{|\btransform\b/, definition_key: 'glossary_term_transform' },
    { pattern: /maximum node count|exceeded/i, definition_key: 'glossary_term_node_limit' },
];

export interface GlossaryMatch {
    start: number;
    end: number;
    matched_text: string;
    definition_key: StringKey;
}

export function find_glossary_matches(message: string): GlossaryMatch[] {
    const candidates: GlossaryMatch[] = [];
    for (const term of GLOSSARY_TERMS) {
        const result = term.pattern.exec(message);
        if (!result) continue;
        candidates.push({
            start: result.index,
            end: result.index + result[0].length,
            matched_text: result[0],
            definition_key: term.definition_key,
        });
    }
    candidates.sort((a, b) => a.start - b.start);

    const matches: GlossaryMatch[] = [];
    let last_end = -1;
    for (const m of candidates) {
        if (m.start < last_end) continue;
        matches.push(m);
        last_end = m.end;
    }
    return matches;
}
