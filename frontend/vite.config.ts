import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';

export default defineConfig({
    base: '/wasmwasm/',
    plugins: [react()],
    build: {
        rollupOptions: {
            input: {
                main: 'index.html',
            },
        },
        rolldownOptions: {
            output: {
                manualChunks(id) {
                    if (
                        id.includes('node_modules/react') ||
                        id.includes('node_modules/react-dom')
                    ) {
                        return 'react';
                    }
                    if (
                        id.includes('node_modules/@codemirror') ||
                        id.includes('node_modules/@lezer')
                    ) {
                        return 'codemirror';
                    }
                    if (id.includes('node_modules/@wasmer')) {
                        return 'wasmer';
                    }
                    if (id.includes('node_modules/@xyflow')) {
                        return 'xyflow';
                    }
                    if (id.includes('node_modules/@bufbuild')) {
                        return 'bufbuild';
                    }
                },
            },
        },
    },
});
