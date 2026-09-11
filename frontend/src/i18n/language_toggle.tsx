import { useLang } from './lang_context';
import './language_toggle.scss';

export function LanguageToggle() {
    const { lang, set_lang } = useLang();

    return (
        <div className="lang-toggle">
            <button
                className={
                    'lang-toggle__option' + (lang === 'en' ? ' lang-toggle__option--active' : '')
                }
                onClick={() => set_lang('en')}
            >
                EN
            </button>
            <button
                className={
                    'lang-toggle__option' + (lang === 'pt' ? ' lang-toggle__option--active' : '')
                }
                onClick={() => set_lang('pt')}
            >
                PT
            </button>
        </div>
    );
}
