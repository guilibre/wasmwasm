export interface StringEntry {
    en: string;
    pt: string;
}

export const STRINGS = {
    play: { en: 'Play', pt: 'Tocar' },
    stop: { en: 'Stop', pt: 'Parar' },
    export_patch: { en: 'Export', pt: 'Exportar' },
    import_patch: { en: 'Import', pt: 'Importar' },
    click_to_rename: { en: 'Click to rename', pt: 'Clique para renomear' },

    open_sidebar: { en: 'Open sidebar', pt: 'Abrir sidebar' },
    close_sidebar: { en: 'Close sidebar', pt: 'Fechar sidebar' },
    waveform: { en: 'waveform', pt: 'forma de onda' },
    spectrum: { en: 'spectrum', pt: 'espectro' },

    open_score: { en: 'Open score', pt: 'Abrir score' },
    close_score: { en: 'Close score', pt: 'Fechar score' },

    zoom_vertical_out: { en: 'Zoom out (vertical)', pt: 'Zoom vertical -' },
    zoom_vertical_in: { en: 'Zoom in (vertical)', pt: 'Zoom vertical +' },
    close: { en: 'Close', pt: 'Fechar' },

    redraw: { en: 'redraw', pt: 'redesenhar' },

    bpm_label: { en: 'bpm', pt: 'bpm' },
    no_instruments_found: { en: 'no instruments found', pt: 'nenhum instrumento encontrado' },

    playing: { en: 'playing', pt: 'tocando' },
    stopped: { en: 'stopped', pt: 'parado' },
    cpu_label: { en: 'cpu', pt: 'cpu' },

    global_tab: { en: 'global', pt: 'global' },

    block_name_placeholder: { en: 'block name', pt: 'nome do bloco' },
    remove: { en: 'Remove', pt: 'Remover' },

    invalid_patch_file: { en: 'Invalid patch file.', pt: 'Arquivo de patch inválido.' },
    cannot_read_patch_file: {
        en: 'Could not read the patch file.',
        pt: 'Não foi possível ler o arquivo de patch.',
    },
    cannot_save_patch_locally: {
        en: 'Could not save the patch locally (storage unavailable or full).',
        pt: 'Não foi possível salvar o patch localmente (armazenamento indisponível ou cheio).',
    },

    line_label: { en: 'Line', pt: 'Linha' },
    col_label: { en: 'Col', pt: 'Col' },

    score_examples_button: { en: 'examples', pt: 'exemplos' },
    score_examples_title: { en: 'Score examples', pt: 'Exemplos de score' },
    score_example_confirm_overwrite: {
        en: 'Replace the current score with this example? (Ctrl+Z undoes this.)',
        pt: 'Substituir o score atual por este exemplo? (Ctrl+Z desfaz isso.)',
    },

    score_example_01_title: { en: 'Attach an instrument', pt: 'Associar um instrumento' },
    score_example_01_desc: {
        en: "'@' merges an instrument onto the note so it can actually sound.",
        pt: "'@' aplica um instrumento à nota para que ela realmente soe.",
    },
    score_example_02_title: { en: 'Sequence', pt: 'Sequência' },
    score_example_02_desc: {
        en: 'Terms written one after another play in order.',
        pt: 'Termos escritos em sequência tocam em ordem.',
    },
    score_example_03_title: { en: 'Named melody', pt: 'Melodia nomeada' },
    score_example_03_desc: {
        en: 'Pull a sequence into its own name so it can be reused.',
        pt: 'Extrai uma sequência para um nome próprio, para poder reutilizá-la.',
    },
    score_example_04_title: { en: 'Adjust timing', pt: 'Ajustar o tempo' },
    score_example_04_desc: {
        en: "'!' scales duration - here, every note lasts half as long.",
        pt: "'!' escala a duração - aqui, cada nota dura metade do tempo.",
    },
    score_example_05_title: { en: 'Two voices in parallel', pt: 'Duas vozes em paralelo' },
    score_example_05_desc: {
        en: "'&' forks playback into two voices that play at the same time.",
        pt: "'&' bifurca a reprodução em duas vozes que tocam ao mesmo tempo.",
    },
    score_example_06_title: { en: 'Transform pitch', pt: 'Transformar a altura' },
    score_example_06_desc: {
        en: "'@{field: expr}' rewrites a parameter for everything inside it.",
        pt: "'@{campo: expr}' reescreve um parâmetro para tudo que está dentro.",
    },
    score_example_07_title: { en: 'Loop forever', pt: 'Loop infinito' },
    score_example_07_desc: {
        en: 'A composition that refers to itself keeps playing forever.',
        pt: 'Uma composição que se refere a si mesma continua tocando para sempre.',
    },
    score_example_08_title: { en: 'Conditional loop', pt: 'Loop condicional' },
    score_example_08_desc: {
        en: "'choose' combined with self-reference lets a loop stop instead of running forever.",
        pt: "'choose' combinado com auto-referência permite que um loop pare em vez de rodar para sempre.",
    },
    score_example_09_title: { en: 'Signal between voices', pt: 'Sinal entre vozes' },
    score_example_09_desc: {
        en: "'emit'/'listen' let two independent voices communicate live during playback.",
        pt: "'emit'/'listen' permitem que duas vozes independentes se comuniquem ao vivo durante a reprodução.",
    },

    glossary_term_const: {
        en: "A 'const' value is fixed at parse time and can't change while the score plays.",
        pt: "Um valor 'const' é fixo em tempo de compilação e não muda enquanto o score toca.",
    },
    glossary_term_scale: {
        en: 'A named list of pitch offsets (e.g. major, minor) that note letters are resolved against.',
        pt: 'Uma lista nomeada de deslocamentos de altura (ex: maior, menor) usada para resolver as notas.',
    },
    glossary_term_undefined_variable: {
        en: "The name you used hasn't been declared anywhere before this point in the score.",
        pt: 'O nome usado não foi declarado em nenhum lugar antes deste ponto do score.',
    },
    glossary_term_circular_reference: {
        en: "A definition that (directly or indirectly) refers to itself while it's still being resolved.",
        pt: 'Uma definição que se refere a si mesma (direta ou indiretamente) enquanto ainda está sendo resolvida.',
    },
    glossary_term_repeat: {
        en: "The 'repeat' pipe operator plays a sequence a fixed number of times in a row.",
        pt: "O operador 'repeat' toca uma sequência um número fixo de vezes em seguida.",
    },
    glossary_term_reverse: {
        en: "The 'reverse' pipe operator plays a sequence back to front.",
        pt: "O operador 'reverse' toca uma sequência de trás para frente.",
    },
    glossary_term_skip: {
        en: "The 'skip' pipe operator jumps ahead past a number of notes/beats before playing.",
        pt: "O operador 'skip' avança, pulando um número de notas/tempos antes de tocar.",
    },
    glossary_term_pipe: {
        en: "The '|>' pipe operator passes a sequence into a transform like repeat/reverse/skip.",
        pt: "O operador de pipe '|>' passa uma sequência para uma transformação como repeat/reverse/skip.",
    },
    glossary_term_fork: {
        en: "The '&' operator plays two or more voices in parallel (a 'fork').",
        pt: "O operador '&' toca duas ou mais vozes em paralelo (um 'fork').",
    },
    glossary_term_legato: {
        en: "The '~' operator ties two notes together into one continuous sound (legato).",
        pt: "O operador '~' liga duas notas em um único som contínuo (legato).",
    },
    glossary_term_sequence: {
        en: 'A sequence is one or more terms played one after another.',
        pt: 'Uma sequência é um ou mais termos tocados um após o outro.',
    },
    glossary_term_atomic: {
        en: "An 'atomic' variable can only ever be redefined with a fixed (non-expression) value.",
        pt: "Uma variável 'atomic' só pode ser redefinida com um valor fixo (não uma expressão).",
    },
    glossary_term_transform: {
        en: "'@{...}' pushes new parameter values (like instrument or freq) onto a term.",
        pt: "'@{...}' aplica novos valores de parâmetro (como instrument ou freq) a um termo.",
    },
    glossary_term_node_limit: {
        en: 'This usually means an infinite loop, often a repeat/self-reference whose stop condition never becomes true.',
        pt: 'Isso geralmente indica um loop infinito, normalmente um repeat/auto-referência cuja condição de parada nunca se torna verdadeira.',
    },
} satisfies Record<string, StringEntry>;

export type StringKey = keyof typeof STRINGS;
