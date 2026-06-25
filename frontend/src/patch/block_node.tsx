import { useEffect } from 'react';
import { Handle, Position, useUpdateNodeInternals } from '@xyflow/react';
import type { NodeProps } from '@xyflow/react';
import type { BlockData } from './use_patch_store';

export function BlockNode({ id, data, selected }: NodeProps) {
    const { name, num_inputs, num_outputs, params } = data as unknown as BlockData;
    const update_node_internals = useUpdateNodeInternals();
    useEffect(() => {
        update_node_internals(id);
    }, [id, num_inputs, num_outputs, update_node_internals]);

    return (
        <div className={`ww-node ww-node--block${selected ? ' ww-node--selected' : ''}`}>
            {Array.from({ length: num_inputs }, (_, i) => (
                <Handle
                    key={`in_${i}`}
                    id={`in_${i}`}
                    type="target"
                    position={Position.Top}
                    style={{ left: `${((i + 1) / (num_inputs + 1)) * 100}%` }}
                    className="ww-handle ww-handle--in"
                />
            ))}
            {params && params.length > 0 && (
                <div className="ww-node__params">
                    {params.map((p) => (
                        <span key={p} className="ww-node__param">
                            {p}
                        </span>
                    ))}
                </div>
            )}
            <div className="ww-node__label">{name}</div>
            {Array.from({ length: num_outputs }, (_, i) => (
                <Handle
                    key={`out_${i}`}
                    id={`out_${i}`}
                    type="source"
                    position={Position.Bottom}
                    style={{ left: `${((i + 1) / (num_outputs + 1)) * 100}%` }}
                    className="ww-handle ww-handle--out"
                />
            ))}
        </div>
    );
}
