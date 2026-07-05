import { BaseEdge } from '@xyflow/react';
import type { EdgeProps } from '@xyflow/react';

export function SelfLoopEdge({
    id,
    sourceX,
    sourceY,
    targetX,
    targetY,
    markerEnd,
    style,
}: EdgeProps) {
    const offset = 40;
    const path = `M ${sourceX} ${sourceY} C ${sourceX} ${sourceY + offset}, ${targetX} ${targetY - offset}, ${targetX} ${targetY}`;

    return <BaseEdge id={id} path={path} markerEnd={markerEnd} style={style} />;
}
