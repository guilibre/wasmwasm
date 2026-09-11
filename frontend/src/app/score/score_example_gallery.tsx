import { useT } from '../../i18n/lang_context';
import { SCORE_EXAMPLES, type ScoreExample } from '../../score_examples';
import './score_example_gallery.scss';

interface Props {
    on_select: (source: string) => void;
    on_close: () => void;
}

export function ScoreExampleGallery({ on_select, on_close }: Props) {
    const t = useT();

    const handle_select = (example: ScoreExample) => {
        on_select(example.source);
        on_close();
    };

    return (
        <div className="score-example-gallery">
            <div className="score-example-gallery__header">
                <span className="score-example-gallery__title">{t('score_examples_title')}</span>
                <button className="score-example-gallery__close" onClick={on_close}>
                    ×
                </button>
            </div>
            <div className="score-example-gallery__list">
                {SCORE_EXAMPLES.map((example) => (
                    <button
                        key={example.id}
                        className="score-example-gallery__item"
                        onClick={() => handle_select(example)}
                    >
                        <span className="score-example-gallery__item-title">
                            {t(example.title_key)}
                        </span>
                        <span className="score-example-gallery__item-desc">
                            {t(example.description_key)}
                        </span>
                    </button>
                ))}
            </div>
        </div>
    );
}
