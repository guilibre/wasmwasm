export function parse_fn_sigs(
    ts: typeof import('typescript'),
    code: string,
): { fn_names: string[]; fn_sigs: string[] } {
    const source = ts.createSourceFile('instr.ts', code, ts.ScriptTarget.ES2025);
    const fn_names: string[] = [];
    const fn_sigs: string[] = [];
    for (const stmt of source.statements) {
        if (ts.isFunctionDeclaration(stmt) && stmt.name) {
            fn_names.push(stmt.name.text);
            fn_sigs.push(
                stmt.parameters
                    .map((p) => {
                        const name = ts.isIdentifier(p.name) ? p.name.text : '_';
                        const type_str = p.type ? p.type.getText(source) : 'unknown';
                        const opt = p.initializer || p.questionToken ? '?' : '';
                        return `${name}${opt}: ${type_str}`;
                    })
                    .join(', '),
            );
        }
    }
    return { fn_names, fn_sigs };
}
