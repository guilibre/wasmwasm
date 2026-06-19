import { Handle, Position } from '@xyflow/react';
import type { NodeProps } from '@xyflow/react';

export function DacNode({ selected }: NodeProps) {
    return (
        <div className={`ww-node ww-node--system${selected ? ' ww-node--selected' : ''}`}>
            <Handle
                id="dac_l"
                type="target"
                position={Position.Top}
                style={{ left: '33%' }}
                className="ww-handle ww-handle--in"
            />
            <Handle
                id="dac_r"
                type="target"
                position={Position.Top}
                style={{ left: '67%' }}
                className="ww-handle ww-handle--in"
            />
            <div className="ww-node__label">dac</div>
        </div>
    );
}
