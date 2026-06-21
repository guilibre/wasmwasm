import { useEffect, useRef, useState, useCallback } from 'react';
import { ReactFlowProvider } from '@xyflow/react';
import workletUrl from '../audio/processor.worklet.js?url';
import WasmWasm from '../audio/compiler';
import Editor, { type EditorHandle } from './editor';
import { Sidebar } from './sidebar';
import { PatchEditor } from '../patch/patch_editor';
import { usePatchStore } from '../patch/use_patch_store';
import { patch_to_json } from '../patch/patch_to_json';
import { patch_to_hash, hash_to_patch } from '../patch/share';
import './app.scss';

export default function App() {
    const audioContextRef = useRef<AudioContext | null>(null);
    const workletNodeRef = useRef<AudioWorkletNode | null>(null);
    const micSourceRef = useRef<MediaStreamAudioSourceNode | null>(null);
    const analyser_l_ref = useRef<AnalyserNode | null>(null);
    const analyser_r_ref = useRef<AnalyserNode | null>(null);
    const splitter_ref = useRef<ChannelSplitterNode | null>(null);
    const [analysers, set_analysers] = useState<{ l: AnalyserNode; r: AnalyserNode } | null>(null);
    const [is_playing, set_is_playing] = useState(false);
    const [sidebar_visible, set_sidebar_visible] = useState(true);
    const [error, set_error] = useState<string | null>(null);
    const import_ref = useRef<HTMLInputElement>(null);
    const editor_ref = useRef<EditorHandle>(null);

    const store = usePatchStore();
    const {
        selected_node,
        update_code,
        update_name,
        select,
        export_patch,
        import_patch,
        load_patch,
    } = store;
    const selected_block = selected_node?.type === 'block' ? selected_node : null;

    const [share_label, set_share_label] = useState<'Share' | 'Copied!'>('Share');

    const share = useCallback(async () => {
        const hash = await patch_to_hash(store.nodes, store.edges);
        window.location.hash = hash;
        await navigator.clipboard.writeText(window.location.href);
        set_share_label('Copied!');
        setTimeout(() => set_share_label('Share'), 2000);
    }, [store.nodes, store.edges]);

    useEffect(() => {
        const hash = window.location.hash.slice(1);
        if (!hash) return;
        hash_to_patch(hash).then((result) => {
            if (result) {
                load_patch(result.nodes, result.edges);
                history.replaceState(null, '', window.location.pathname + window.location.search);
            }
        });
    }, [load_patch]);

    const [editing_block_id, set_editing_block_id] = useState<string | null>(null);
    const [name_draft, set_name_draft] = useState('');

    const editing_name = editing_block_id === selected_block?.id;

    const start_name_edit = useCallback(() => {
        set_name_draft((selected_block?.data as { name: string })?.name ?? '');
        set_editing_block_id(selected_block?.id ?? null);
    }, [selected_block]);

    const commit_name = useCallback(() => {
        const trimmed = name_draft.trim();
        if (trimmed && selected_block) update_name(selected_block.id, trimmed);
        set_editing_block_id(null);
    }, [name_draft, selected_block, update_name]);

    const on_name_key_down = useCallback(
        (e: React.KeyboardEvent<HTMLInputElement>) => {
            if (e.key === 'Enter') commit_name();
            else if (e.key === 'Escape') set_editing_block_id(null);
        },
        [commit_name],
    );

    useEffect(() => {
        WasmWasm.ensureReady().then(() => editor_ref.current?.refresh());
    }, []);

    const needs_capture = (patch_json: string) => patch_json.includes('capture_');

    const setup_capture = async (context: AudioContext, node: AudioWorkletNode) => {
        if (micSourceRef.current) return;
        let stream: MediaStream;
        try {
            stream = await navigator.mediaDevices.getUserMedia({
                audio: { echoCancellation: false, noiseSuppression: false, autoGainControl: false },
                video: false,
            });
        } catch (e) {
            throw new Error(`Microphone unavailable: ${e instanceof Error ? e.message : e}`, {
                cause: e,
            });
        }
        const source = context.createMediaStreamSource(stream);
        source.connect(node);
        micSourceRef.current = source;
    };

    const play = async () => {
        set_error(null);
        const json = patch_to_json(store.nodes, store.edges);

        if (!audioContextRef.current) {
            const context = new AudioContext({ latencyHint: 'interactive' });
            console.log('audio latency:', context.baseLatency, context.outputLatency);
            await context.audioWorklet.addModule(workletUrl);
            const node = new AudioWorkletNode(context, 'wasm-processor', {
                numberOfInputs: 1,
                numberOfOutputs: 1,
                outputChannelCount: [2],
            });

            const splitter = context.createChannelSplitter(2);
            const al = context.createAnalyser();
            const ar = context.createAnalyser();
            al.fftSize = 4096;
            ar.fftSize = 4096;
            al.smoothingTimeConstant = 0.75;
            ar.smoothingTimeConstant = 0.75;
            node.connect(splitter);
            splitter.connect(al, 0);
            splitter.connect(ar, 1);
            node.connect(context.destination);

            analyser_l_ref.current = al;
            analyser_r_ref.current = ar;
            splitter_ref.current = splitter;
            set_analysers({ l: al, r: ar });

            audioContextRef.current = context;
            workletNodeRef.current = node;
        }

        if (sidebar_visible && analyser_l_ref.current && analyser_r_ref.current)
            set_analysers({ l: analyser_l_ref.current, r: analyser_r_ref.current });

        let wasm: Uint8Array;
        try {
            wasm = await WasmWasm.init_patch(audioContextRef.current.sampleRate, json);
        } catch (e) {
            set_error(String(e));
            return;
        }

        const context = audioContextRef.current!;
        const node = workletNodeRef.current!;

        if (needs_capture(json)) await setup_capture(context, node);

        node.port.postMessage({ type: 'load-wasm', buffer: wasm });
        await context.resume();
        set_is_playing(true);
    };

    const toggle_sidebar = useCallback(() => {
        const al = analyser_l_ref.current;
        const ar = analyser_r_ref.current;
        const splitter = splitter_ref.current;
        if (sidebar_visible) {
            al?.disconnect();
            ar?.disconnect();
            set_analysers(null);
        } else {
            if (is_playing && splitter && al && ar) {
                splitter.connect(al, 0);
                splitter.connect(ar, 1);
                set_analysers({ l: al, r: ar });
            }
        }
        set_sidebar_visible((v) => !v);
    }, [sidebar_visible, is_playing]);

    const stop = async () => {
        if (micSourceRef.current) {
            micSourceRef.current.mediaStream.getTracks().forEach((t) => t.stop());
            micSourceRef.current.disconnect();
            micSourceRef.current = null;
        }
        await audioContextRef.current?.suspend();
        set_is_playing(false);
        set_analysers(null);
    };

    return (
        <div className="app">
            <div className="app__toolbar">
                <span className="app__brand">wasmwasm</span>
                <button onClick={is_playing ? stop : play}>{is_playing ? 'Stop' : 'Play'}</button>
                <button onClick={share}>{share_label}</button>
                <button onClick={export_patch}>Export</button>
                <button onClick={() => import_ref.current?.click()}>Import</button>
                <input
                    ref={import_ref}
                    type="file"
                    accept=".json"
                    style={{ display: 'none' }}
                    onChange={(e) => {
                        const file = e.target.files?.[0];
                        if (file) import_patch(file);
                        e.target.value = '';
                    }}
                />
                {error && <span className="app__error">{error}</span>}
            </div>

            <div className="app__workspace">
                <ReactFlowProvider>
                    <PatchEditor store={store} />
                </ReactFlowProvider>
                {sidebar_visible ? (
                    <Sidebar
                        analyser_l={analysers?.l ?? null}
                        analyser_r={analysers?.r ?? null}
                        on_close={toggle_sidebar}
                    />
                ) : (
                    <button className="app__sidebar-open" onClick={toggle_sidebar}>
                        ‹
                    </button>
                )}
            </div>

            {selected_block && (
                <div className="app__modal-backdrop" onClick={() => select(null)}>
                    <div className="app__modal" onClick={(e) => e.stopPropagation()}>
                        <div className="app__panel-header">
                            {editing_name ? (
                                <input
                                    className="app__name-input"
                                    autoFocus
                                    value={name_draft}
                                    onChange={(e) => set_name_draft(e.target.value)}
                                    onKeyDown={on_name_key_down}
                                    onBlur={commit_name}
                                />
                            ) : (
                                <span
                                    className="app__panel-name"
                                    onClick={start_name_edit}
                                    title="Click to rename"
                                >
                                    {(selected_block.data as { name: string }).name}
                                </span>
                            )}
                            <button onClick={() => select(null)}>×</button>
                        </div>
                        <Editor
                            ref={editor_ref}
                            key={selected_block.id}
                            initialValue={(selected_block.data as { code: string }).code}
                            onChange={(code) => update_code(selected_block.id, code)}
                        />
                    </div>
                </div>
            )}
        </div>
    );
}
