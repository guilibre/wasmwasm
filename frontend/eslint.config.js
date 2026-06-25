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
        rules: {
            ...html.configs['flat/recommended'].rules,
            '@html-eslint/attrs-newline': 'off',
            '@html-eslint/no-extra-spacing-tags': 'off',
            '@html-eslint/require-closing-tags': ['error', { selfClosing: 'always' }],
        },
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
            '@typescript-eslint/naming-convention': [
                'error',
                { selector: 'default', format: ['snake_case'] },
                {
                    selector: 'import',
                    format: ['camelCase', 'PascalCase', 'snake_case', 'UPPER_CASE'],
                },
                { selector: 'typeLike', format: ['PascalCase'] },
                { selector: 'enumMember', format: ['PascalCase', 'UPPER_CASE'] },
                {
                    selector: 'variable',
                    format: ['snake_case', 'UPPER_CASE', 'PascalCase'],
                },
                {
                    selector: 'function',
                    format: ['snake_case', 'PascalCase'],
                },
                {
                    selector: 'function',
                    modifiers: ['exported'],
                    format: ['snake_case', 'PascalCase'],
                },
                {
                    selector: ['variable', 'function'],
                    format: ['camelCase'],
                    filter: { regex: '^use[A-Z]', match: true },
                },
                {
                    selector: 'parameter',
                    format: ['snake_case'],
                    leadingUnderscore: 'allow',
                },
                {
                    selector: 'objectLiteralProperty',
                    format: null,
                },
                {
                    selector: 'typeProperty',
                    format: null,
                },
                {
                    selector: ['variable', 'parameter'],
                    modifiers: ['destructured'],
                    format: null,
                },
                {
                    selector: 'classProperty',
                    format: ['snake_case', 'UPPER_CASE'],
                    leadingUnderscore: 'allow',
                },
                {
                    selector: 'classMethod',
                    format: ['snake_case', 'camelCase'],
                },
                {
                    selector: 'objectLiteralMethod',
                    format: ['snake_case', 'camelCase'],
                },
            ],
        },
        languageOptions: {
            ecmaVersion: 2020,
            globals: globals.browser,
        },
    },
]);
