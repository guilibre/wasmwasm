import { useCallback, useEffect, useRef, useState } from 'react';
import type { VirtualTypeScriptEnvironment } from '@typescript/vfs';
import WasmWasm, { type CompiledPatch } from '../audio/compiler';
import { patch_to_json } from '../patch/patch_to_json';
import type { OrchestraState } from '../patch/use_patch_store';
import {
    make_orchestra_env_with_instruments,
    make_instrument_env_with_params,
    get_orchestra_env,
    type CompiledInstrument,
} from './ts_env';
import { parse_fn_sigs } from './parse_fn_sigs';
import { GLOBAL_CACHE_KEY } from './constants';

export function useInstrumentCompiler(
    orchestra: OrchestraState,
    set_instrument_code: (id: string, code: string) => void,
    set_global_patch_code: (code: string) => void,
    set_error: (error: string | null) => void,
) {
    const ts_ref = useRef<typeof import('typescript') | null>(null);
    const sig_parse_timers_ref = useRef<Map<string, ReturnType<typeof setTimeout>>>(new Map());
    const patch_cache_ref = useRef<Map<string, { json: string; compiled: CompiledPatch }>>(
        new Map(),
    );

    const [compile_status, set_compile_status] = useState<
        Map<string, 'idle' | 'compiling' | 'ok' | 'error'>
    >(new Map());
    const [compiled_patch_params, set_compiled_patch_params] = useState<Map<string, string[]>>(
        new Map(),
    );
    const [compiled_instrument_info, set_compiled_instrument_info] = useState<
        Map<string, CompiledInstrument>
    >(new Map());
    const [orchestra_env, set_orchestra_env] = useState<VirtualTypeScriptEnvironment | null>(() =>
        get_orchestra_env(),
    );
    const [instrument_envs, set_instrument_envs] = useState<
        Map<string, VirtualTypeScriptEnvironment>
    >(new Map());

    const initial_instruments_ref = useRef(orchestra.instruments);
    const initial_global_patch_code_ref = useRef(orchestra.global_patch_code);

    const set_status = useCallback((key: string, status: 'idle' | 'compiling' | 'ok' | 'error') => {
        set_compile_status((prev) => new Map(prev).set(key, status));
    }, []);

    const prev_patch_data = useRef<Map<string, { nodes: unknown; edges: unknown }>>(new Map());
    useEffect(() => {
        for (const instr of orchestra.instruments) {
            const prev = prev_patch_data.current.get(instr.id);
            if (prev && (prev.nodes !== instr.nodes || prev.edges !== instr.edges)) {
                patch_cache_ref.current.delete(instr.id);
                set_compile_status((s) => {
                    const m = new Map(s);
                    m.delete(`patch:${instr.id}`);
                    return m;
                });
            }
            prev_patch_data.current.set(instr.id, { nodes: instr.nodes, edges: instr.edges });
        }
    }, [orchestra.instruments]);

    const prev_global_data_ref = useRef<{ nodes: unknown; edges: unknown } | null>(null);
    useEffect(() => {
        const prev = prev_global_data_ref.current;
        if (
            prev &&
            (prev.nodes !== orchestra.global_nodes || prev.edges !== orchestra.global_edges)
        ) {
            patch_cache_ref.current.delete(GLOBAL_CACHE_KEY);
        }
        prev_global_data_ref.current = {
            nodes: orchestra.global_nodes,
            edges: orchestra.global_edges,
        };
    }, [orchestra.global_nodes, orchestra.global_edges]);

    const handle_instrument_code_change = useCallback(
        (id: string, code: string) => {
            set_instrument_code(id, code);
            set_compile_status((s) => {
                const m = new Map(s);
                m.delete(`instr:${id}`);
                return m;
            });
            if (ts_ref.current) {
                const existing_timer = sig_parse_timers_ref.current.get(id);
                if (existing_timer !== undefined) clearTimeout(existing_timer);
                const timer = setTimeout(() => {
                    sig_parse_timers_ref.current.delete(id);
                    if (!ts_ref.current) return;
                    const { fn_names, fn_sigs } = parse_fn_sigs(ts_ref.current, code);
                    set_compiled_instrument_info((prev) => {
                        const existing = prev.get(id);
                        const new_map = new Map(prev).set(id, {
                            name: existing?.name ?? id,
                            param_names: existing?.param_names ?? [],
                            fn_names,
                            fn_sigs,
                        });
                        const sigs_changed =
                            !existing ||
                            existing.fn_names.length !== fn_names.length ||
                            existing.fn_names.some((f, i) => f !== fn_names[i]) ||
                            existing.fn_sigs.some((s, i) => s !== fn_sigs[i]);
                        if (sigs_changed) {
                            set_orchestra_env(
                                make_orchestra_env_with_instruments([...new_map.values()]),
                            );
                        }
                        return new_map;
                    });
                }, 2000);
                sig_parse_timers_ref.current.set(id, timer);
            }
        },
        [set_instrument_code],
    );

    const handle_global_patch_code_change = useCallback(
        (code: string) => {
            set_global_patch_code(code);
            set_compile_status((s) => {
                const m = new Map(s);
                m.delete(`instr:${GLOBAL_CACHE_KEY}`);
                return m;
            });
            if (ts_ref.current) {
                const existing_timer = sig_parse_timers_ref.current.get(GLOBAL_CACHE_KEY);
                if (existing_timer !== undefined) clearTimeout(existing_timer);
                const timer = setTimeout(() => {
                    sig_parse_timers_ref.current.delete(GLOBAL_CACHE_KEY);
                    if (!ts_ref.current) return;
                    const { fn_names, fn_sigs } = parse_fn_sigs(ts_ref.current, code);
                    set_compiled_instrument_info((prev) => {
                        const existing = prev.get(GLOBAL_CACHE_KEY);
                        const new_map = new Map(prev).set(GLOBAL_CACHE_KEY, {
                            name: 'global',
                            param_names: existing?.param_names ?? [],
                            fn_names,
                            fn_sigs,
                        });
                        const sigs_changed =
                            !existing ||
                            existing.fn_names.length !== fn_names.length ||
                            existing.fn_names.some((f, i) => f !== fn_names[i]) ||
                            existing.fn_sigs.some((s, i) => s !== fn_sigs[i]);
                        if (sigs_changed) {
                            set_orchestra_env(
                                make_orchestra_env_with_instruments([...new_map.values()]),
                            );
                        }
                        return new_map;
                    });
                }, 2000);
                sig_parse_timers_ref.current.set(GLOBAL_CACHE_KEY, timer);
            }
        },
        [set_global_patch_code],
    );

    useEffect(() => {
        import('typescript').then((ts) => {
            ts_ref.current = ts;
            const new_map = new Map<string, CompiledInstrument>();
            for (const instr of initial_instruments_ref.current) {
                const { fn_names, fn_sigs } = parse_fn_sigs(ts, instr.code);
                new_map.set(instr.id, {
                    name: instr.name,
                    param_names: [],
                    fn_names,
                    fn_sigs,
                });
            }
            const { fn_names, fn_sigs } = parse_fn_sigs(ts, initial_global_patch_code_ref.current);
            new_map.set(GLOBAL_CACHE_KEY, {
                name: 'global',
                param_names: [],
                fn_names,
                fn_sigs,
            });
            set_compiled_instrument_info(new_map);
            set_orchestra_env(make_orchestra_env_with_instruments([...new_map.values()]));
        });
    }, []);

    const compile_patch = useCallback(
        async (sample_rate: number) => {
            set_error(null);
            await Promise.all([
                ...orchestra.instruments.map(async (instr) => {
                    const key = `patch:${instr.id}`;
                    set_status(key, 'compiling');
                    try {
                        const json = patch_to_json(instr.nodes, instr.edges);
                        const compiled = await WasmWasm.compile_patch(sample_rate, json);
                        patch_cache_ref.current.set(instr.id, { json, compiled });
                        set_compiled_patch_params((prev) =>
                            new Map(prev).set(instr.id, compiled.param_names),
                        );
                        const env = make_instrument_env_with_params(compiled.param_names);
                        set_instrument_envs((prev) => new Map(prev).set(instr.id, env));
                        set_status(key, 'ok');
                    } catch (e) {
                        set_error(`[patch:${instr.name}] ${String(e)}`);
                        set_status(key, 'error');
                    }
                }),
                (async () => {
                    const key = `patch:${GLOBAL_CACHE_KEY}`;
                    set_status(key, 'compiling');
                    try {
                        const json = patch_to_json(orchestra.global_nodes, orchestra.global_edges);
                        const compiled = await WasmWasm.compile_patch(sample_rate, json);
                        patch_cache_ref.current.set(GLOBAL_CACHE_KEY, { json, compiled });
                        set_compiled_patch_params((prev) =>
                            new Map(prev).set(GLOBAL_CACHE_KEY, compiled.param_names),
                        );
                        const env = make_instrument_env_with_params(compiled.param_names);
                        set_instrument_envs((prev) => new Map(prev).set(GLOBAL_CACHE_KEY, env));
                        set_status(key, 'ok');
                    } catch (e) {
                        set_error(`[global] ${String(e)}`);
                        set_status(key, 'error');
                    }
                })(),
            ]);
        },
        [
            orchestra.instruments,
            orchestra.global_nodes,
            orchestra.global_edges,
            set_error,
            set_status,
        ],
    );

    const compile_instrument = useCallback(async () => {
        set_error(null);
        const ts = await import('typescript');
        let new_info_map = new Map(compiled_instrument_info);
        await Promise.all([
            ...orchestra.instruments.map(async (instr) => {
                const key = `instr:${instr.id}`;
                set_status(key, 'compiling');
                try {
                    const { fn_names, fn_sigs } = parse_fn_sigs(ts, instr.code);
                    const param_names = compiled_patch_params.get(instr.id) ?? [];
                    new_info_map = new Map(new_info_map).set(instr.id, {
                        name: instr.name,
                        param_names,
                        fn_names,
                        fn_sigs,
                    });
                    set_status(key, 'ok');
                } catch (e) {
                    set_error(`[instr:${instr.name}] ${String(e)}`);
                    set_status(key, 'error');
                }
            }),
            (async () => {
                const key = `instr:${GLOBAL_CACHE_KEY}`;
                set_status(key, 'compiling');
                try {
                    const { fn_names, fn_sigs } = parse_fn_sigs(ts, orchestra.global_patch_code);
                    const param_names = compiled_patch_params.get(GLOBAL_CACHE_KEY) ?? [];
                    new_info_map = new Map(new_info_map).set(GLOBAL_CACHE_KEY, {
                        name: 'global',
                        param_names,
                        fn_names,
                        fn_sigs,
                    });
                    set_status(key, 'ok');
                } catch (e) {
                    set_error(`[global] ${String(e)}`);
                    set_status(key, 'error');
                }
            })(),
        ]);
        set_compiled_instrument_info(new_info_map);
        const env = make_orchestra_env_with_instruments([...new_info_map.values()]);
        set_orchestra_env(env);
    }, [
        orchestra.instruments,
        orchestra.global_patch_code,
        compiled_patch_params,
        compiled_instrument_info,
        set_error,
        set_status,
    ]);

    const compile_orchestra = useCallback(async () => {
        set_status('orchestra', 'compiling');
        set_error(null);
        try {
            const ts = await import('typescript');
            ts.transpileModule(orchestra.code, {
                compilerOptions: {
                    target: ts.ScriptTarget.ES2025,
                    module: ts.ModuleKind.ESNext,
                },
            });
            set_status('orchestra', 'ok');
        } catch (e) {
            set_error(`[orchestra] ${String(e)}`);
            set_status('orchestra', 'error');
        }
    }, [orchestra.code, set_error, set_status]);

    return {
        patch_cache_ref,
        compile_status,
        compiled_instrument_info,
        orchestra_env,
        instrument_envs,
        handle_instrument_code_change,
        handle_global_patch_code_change,
        compile_patch,
        compile_instrument,
        compile_orchestra,
    };
}
