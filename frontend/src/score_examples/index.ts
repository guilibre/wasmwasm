import type { StringKey } from '../i18n/strings';

export interface ScoreExample {
    id: string;
    title_key: StringKey;
    description_key: StringKey;
    source: string;
}

const raw_examples = import.meta.glob('./*.score', {
    query: '?raw',
    import: 'default',
    eager: true,
}) as Record<string, string>;

const METADATA: Record<string, { title_key: StringKey; description_key: StringKey }> = {
    attach_instrument: {
        title_key: 'score_example_01_title',
        description_key: 'score_example_01_desc',
    },
    sequence: {
        title_key: 'score_example_02_title',
        description_key: 'score_example_02_desc',
    },
    named_melody: {
        title_key: 'score_example_03_title',
        description_key: 'score_example_03_desc',
    },
    adjust_timing: {
        title_key: 'score_example_04_title',
        description_key: 'score_example_04_desc',
    },
    fork_voices: {
        title_key: 'score_example_05_title',
        description_key: 'score_example_05_desc',
    },
    transform_pitch: {
        title_key: 'score_example_06_title',
        description_key: 'score_example_06_desc',
    },
    loop: {
        title_key: 'score_example_07_title',
        description_key: 'score_example_07_desc',
    },
    conditional_loop: {
        title_key: 'score_example_08_title',
        description_key: 'score_example_08_desc',
    },
    multi_voice_signaling: {
        title_key: 'score_example_09_title',
        description_key: 'score_example_09_desc',
    },
};

export const SCORE_EXAMPLES: ScoreExample[] = Object.entries(raw_examples)
    .map(([path, source]) => ({
        id: path.replace('./', '').replace('.score', ''),
        source,
    }))
    .sort((a, b) => a.id.localeCompare(b.id))
    .map(({ id, source }) => ({ id, source, ...METADATA[id] }));
