import type { ReactNode } from 'react';
import { useT } from '../i18n/lang_context';
import { find_glossary_matches } from './glossary';
import './error_message.scss';

interface Props {
    message: string;
    line?: number;
    col?: number;
}

export function ErrorMessage({ message, line, col }: Props) {
    const t = useT();
    const matches = find_glossary_matches(message);

    const segments: ReactNode[] = [];
    let cursor = 0;
    matches.forEach((m, i) => {
        if (m.start > cursor) segments.push(message.slice(cursor, m.start));
        segments.push(
            <span key={i} className="error-message__term" tabIndex={0}>
                {m.matched_text}
                <span className="error-message__tooltip">{t(m.definition_key)}</span>
            </span>,
        );
        cursor = m.end;
    });
    if (cursor < message.length) segments.push(message.slice(cursor));

    return (
        <span className="error-message">
            {line !== undefined && col !== undefined && (
                <span className="error-message__position">
                    {t('line_label')} {line + 1}, {t('col_label')} {col + 1}:{' '}
                </span>
            )}
            {segments}
        </span>
    );
}
