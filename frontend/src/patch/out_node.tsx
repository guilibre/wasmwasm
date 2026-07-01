import { useEffect } from 'react';
import { Handle, Position, useUpdateNodeInternals } from '@xyflow/react';
import type { NodeProps } from '@xyflow/react';

export function OutNode({ id, data, selected }: NodeProps) {
    const { name, num_channels } = data as unknown as { name: string; num_channels: number };
    const update_node_internals = useUpdateNodeInternals();
    useEffect(() => {
        update_node_internals(id);
    }, [id, num_channels, update_node_internals]);

    return (
        <div className={`ww-node ww-node--system${selected ? ' ww-node--selected' : ''}`}>
            {Array.from({ length: num_channels }, (_, i) => (
                <Handle
                    key={`out_${i}`}
                    id={`out_${i}`}
                    type="target"
                    position={Position.Top}
                    style={{ left: `${((i + 1) / (num_channels + 1)) * 100}%` }}
                    className="ww-handle ww-handle--in"
                />
            ))}
            <div className="ww-node__label">{name}</div>
        </div>
    );
}
