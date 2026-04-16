import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react-swc';

export default defineConfig({
    base: '/wasmwasm/',
    plugins: [react()],
    build: {
        rolldownOptions: {
            output: {
                manualChunks(id) {
                    if (
                        id.includes('node_modules/react') ||
                        id.includes('node_modules/react-dom')
                    ) {
                        return 'react';
                    }
                    if (id.includes('node_modules/@codemirror')) {
                        return 'codemirror';
                    }
                    if (id.includes('node_modules/@wasmer')) {
                        return 'wasmer';
                    }
                },
            },
        },
    },
});
