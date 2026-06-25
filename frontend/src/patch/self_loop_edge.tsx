import { BaseEdge } from '@xyflow/react';
import type { EdgeProps } from '@xyflow/react';

const OFFSET = 40;
const CTRL = 20;

export function SelfLoopEdge({ sourceX, sourceY, targetX, targetY, style, markerEnd }: EdgeProps) {
    const mid_x =
        sourceX < targetX
            ? Math.max(sourceX, targetX) + OFFSET
            : Math.min(sourceX, targetX) - OFFSET;
    const mid_y = (sourceY + targetY) / 2;

    const d = [
        `M ${sourceX} ${sourceY}`,
        `C ${sourceX} ${sourceY + CTRL}, ${mid_x} ${sourceY + CTRL}, ${mid_x} ${mid_y}`,
        `C ${mid_x} ${targetY - CTRL}, ${targetX} ${targetY - CTRL}, ${targetX} ${targetY}`,
    ].join(' ');

    return <BaseEdge path={d} style={style} markerEnd={markerEnd} />;
}
