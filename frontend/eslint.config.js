import js from '@eslint/js';
import globals from 'globals';
import reactHooks from 'eslint-plugin-react-hooks';
import reactRefresh from 'eslint-plugin-react-refresh';
import tseslint from 'typescript-eslint';
import { globalIgnores } from 'eslint/config';
import html from '@html-eslint/eslint-plugin';
import prettier from 'eslint-plugin-prettier';
import prettierConfig from 'eslint-config-prettier';

export default tseslint.config([
    globalIgnores(['dist']),
    {
        ...html.configs['flat/recommended'],
        files: ['**/*.html'],
    },
    {
        files: ['**/*.{ts,tsx}'],
        extends: [js.configs.recommended, tseslint.configs.recommended, reactRefresh.configs.vite],
        plugins: {
            'react-hooks': reactHooks,
            prettier,
        },
        rules: {
            ...reactHooks.configs.recommended.rules,
            ...prettierConfig.rules,
            'prettier/prettier': 'error',
        },
        languageOptions: {
            ecmaVersion: 2020,
            globals: globals.browser,
        },
    },
]);
