import { useEffect } from 'react';
import { Handle, Position, useUpdateNodeInternals } from '@xyflow/react';
import type { NodeProps } from '@xyflow/react';
import { handle_offset } from './handle_layout';

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
                    style={{ left: handle_offset(i, num_channels) }}
                    className="ww-handle ww-handle--in"
                />
            ))}
            <div className="ww-node__label">{name}</div>
        </div>
    );
}
