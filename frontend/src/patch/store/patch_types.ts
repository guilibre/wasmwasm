import type { Node, Edge, NodeChange, EdgeChange, Connection } from '@xyflow/react';

export interface BlockData {
    name: string;
    code: string;
    num_inputs: number;
    num_outputs: number;
    params: string[];
}

export interface OutData {
    name: string;
    num_channels: number;
}

export interface InData {
    name: string;
    num_channels: number;
    num_in_channels: number;
}

export interface InstrumentState {
    id: string;
    nodes: Node[];
    edges: Edge[];
}

export interface OrchestraState {
    bpm: number;
    active_id: string | null;
    instruments: InstrumentState[];
    global_nodes: Node[];
    global_edges: Edge[];
}

export type PatchView = 'instrument' | 'global';

export type ScoreParamBindings = Record<string, string>;

export interface PatchState {
    orchestra: OrchestraState;
    selected_id: string | null;
    view: PatchView;
    score_source: string;
    score_param_bindings: ScoreParamBindings;
    global_callback_source: string;
    load_serial: number;
}

export type PatchAction =
    | { type: 'nodes_change'; changes: NodeChange[] }
    | { type: 'edges_change'; changes: EdgeChange[] }
    | { type: 'connect'; connection: Connection }
    | { type: 'global_nodes_change'; changes: NodeChange[] }
    | { type: 'global_edges_change'; changes: EdgeChange[] }
    | { type: 'global_connect'; connection: Connection }
    | { type: 'set_view'; view: PatchView }
    | { type: 'select'; id: string | null }
    | { type: 'update_code'; id: string; code: string }
    | { type: 'update_name'; id: string; name: string }
    | { type: 'add_node'; node: Node }
    | { type: 'update_global_code'; id: string; code: string }
    | { type: 'update_score_source'; source: string }
    | { type: 'update_score_param_bindings'; bindings: ScoreParamBindings }
    | { type: 'update_global_callback_source'; source: string }
    | { type: 'update_global_name'; id: string; name: string }
    | { type: 'add_global_node'; node: Node }
    | { type: 'set_orchestra_bpm'; bpm: number }
    | { type: 'add_instrument' }
    | { type: 'remove_instrument'; id: string }
    | { type: 'rename_instrument'; id: string; name: string }
    | { type: 'set_active_instrument'; id: string }
    | {
          type: 'load';
          orchestra: OrchestraState;
          score_source?: string;
          score_param_bindings?: ScoreParamBindings;
          global_callback_source?: string;
      }
    | { type: 'apply_layout'; instrument_id: string; nodes: Node[] }
    | { type: 'apply_global_layout'; nodes: Node[] }
    | { type: 'undo' }
    | { type: 'redo' };

export interface HistoryState {
    past: PatchState[];
    present: PatchState;
    future: PatchState[];
    storage_error: string | null;
}
