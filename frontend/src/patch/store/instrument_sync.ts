import type { Node } from '@xyflow/react';
import type { InstrumentState, OutData, InData } from './patch_types';

export const default_nodes = (): Node[] => [
    {
        id: 'out',
        type: 'out',
        position: { x: 0, y: 0 },
        data: { name: 'OUT 2', num_channels: 2 } satisfies OutData,
    },
    {
        id: 'in',
        type: 'instrument_in',
        position: { x: 0, y: 200 },
        data: { name: 'IN 2', num_channels: 2 } satisfies OutData,
    },
];

export const default_global_nodes = (): Node[] => [
    { id: 'master_dac', type: 'dac', position: { x: 0, y: 0 }, data: {} },
    { id: 'master_adc', type: 'adc', position: { x: 0, y: 200 }, data: {} },
];

export function unique_instrument_id(
    desired: string,
    existing: InstrumentState[],
    ignore_id?: string,
): string {
    const taken = new Set(existing.filter((i) => i.id !== ignore_id).map((i) => i.id));
    if (!taken.has(desired)) return desired;
    let n = 2;
    while (taken.has(`${desired}${n}`)) n++;
    return `${desired}${n}`;
}

export function in_node_id(instrument_id: string): string {
    return `in_${instrument_id}`;
}

export function out_channels_of(instrument: InstrumentState): number {
    const out_node = instrument.nodes.find((n) => n.type === 'out');
    return (out_node?.data as Partial<OutData> | undefined)?.num_channels ?? 2;
}

export function in_channels_of(instrument: InstrumentState): number {
    const in_node = instrument.nodes.find((n) => n.type === 'instrument_in');
    return (in_node?.data as Partial<OutData> | undefined)?.num_channels ?? 2;
}

export function make_in_node(instrument: InstrumentState): Node {
    return {
        id: in_node_id(instrument.id),
        type: 'in',
        position: { x: 0, y: 0 },
        data: {
            name: instrument.id,
            num_channels: out_channels_of(instrument),
            num_in_channels: in_channels_of(instrument),
        } satisfies InData,
    };
}

export function sync_global_in_nodes(instruments: InstrumentState[], global_nodes: Node[]): Node[] {
    const instrument_ids = new Set(instruments.map((i) => i.id));
    const kept = global_nodes.filter(
        (n) => n.type !== 'in' || instrument_ids.has(n.id.replace(/^in_/, '')),
    );
    const existing_in_ids = new Set(kept.filter((n) => n.type === 'in').map((n) => n.id));
    const updated = kept.map((n) => {
        if (n.type !== 'in') return n;
        const instrument = instruments.find((i) => in_node_id(i.id) === n.id);
        if (!instrument) return n;
        const num_channels = out_channels_of(instrument);
        const num_in_channels = in_channels_of(instrument);
        const data = n.data as Partial<InData>;
        if (
            data.num_channels === num_channels &&
            data.num_in_channels === num_in_channels &&
            data.name === instrument.id
        )
            return n;
        return { ...n, data: { ...n.data, num_channels, num_in_channels, name: instrument.id } };
    });
    const added = instruments
        .filter((i) => !existing_in_ids.has(in_node_id(i.id)))
        .map((i) => make_in_node(i));
    return [...updated, ...added];
}
