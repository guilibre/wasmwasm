import { Handle, Position } from '@xyflow/react';
import type { NodeProps } from '@xyflow/react';

export function CaptureNode({ selected }: NodeProps) {
    return (
        <div className={`ww-node ww-node--system${selected ? ' ww-node--selected' : ''}`}>
            <div className="ww-node__label">capture</div>
            <Handle
                id="capture_l"
                type="source"
                position={Position.Bottom}
                style={{ left: '0%' }}
                className="ww-handle ww-handle--out"
            />
            <Handle
                id="capture_r"
                type="source"
                position={Position.Bottom}
                style={{ left: '100%' }}
                className="ww-handle ww-handle--out"
            />
        </div>
    );
}
