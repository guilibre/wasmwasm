import { Handle, Position } from '@xyflow/react';
import type { NodeProps } from '@xyflow/react';

export function AdcNode({ selected }: NodeProps) {
    return (
        <div className={`ww-node ww-node--system${selected ? ' ww-node--selected' : ''}`}>
            <div className="ww-node__label">adc</div>
            <Handle
                id="adc_l"
                type="source"
                position={Position.Bottom}
                style={{ left: '0%' }}
                className="ww-handle ww-handle--out"
            />
            <Handle
                id="adc_r"
                type="source"
                position={Position.Bottom}
                style={{ left: '100%' }}
                className="ww-handle ww-handle--out"
            />
        </div>
    );
}
