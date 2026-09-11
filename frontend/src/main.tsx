import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import App from './app/app.tsx';
import { LangProvider } from './i18n/i18n_context';

createRoot(document.getElementById('root')!).render(
    <StrictMode>
        <LangProvider>
            <App />
        </LangProvider>
    </StrictMode>,
);
