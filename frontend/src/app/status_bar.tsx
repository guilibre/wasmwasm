import { useT } from '../i18n/lang_context';

interface StatusBarProps {
    is_playing: boolean;
    cpu_load: number;
}

export function StatusBar({ is_playing, cpu_load }: StatusBarProps) {
    const t = useT();
    const pct = Math.round(cpu_load * 100);
    const bar_pct = Math.min(100, pct);
    const level =
        pct >= 90 ? 'app__statusbar-fill--danger' : pct >= 70 ? 'app__statusbar-fill--warn' : '';

    return (
        <div className="app__statusbar">
            <span className="app__statusbar-label">{is_playing ? t('playing') : t('stopped')}</span>
            <div className="app__statusbar-meter">
                <div className="app__statusbar-track">
                    <div
                        className={`app__statusbar-fill ${level}`}
                        style={{ width: `${bar_pct}%` }}
                    />
                </div>
                <span className="app__statusbar-value">
                    {t('cpu_label')} {pct}%
                </span>
            </div>
        </div>
    );
}
