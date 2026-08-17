import { Elm } from './Main.elm';
import { initSidebarRenderer, setAnalysers, zoomBy } from './sidebarRenderer.js';
import { initScoreEditor } from './scorePanelRenderer.js';
import { createAudioEngine } from './audioRenderer.js';
import ScoreWasm from './scorewasm/compiler';

const app = Elm.Main.init({
    node: document.getElementById('app'),
});

const audio = createAudioEngine({
    onCpu: (load) => app.ports.audioCpu.send(load),
    onPlaying: (playing) => app.ports.audioPlaying.send(playing),
    onError: (message) => app.ports.audioError.send(message),
    onAnalysers: (analysers) => setAnalysers(analysers),
});

app.ports.audioPlay.subscribe((payload) => {
    audio.play(payload);
});

app.ports.audioStop.subscribe(() => {
    audio.stop();
});

app.ports.audioSetBpm.subscribe((bpm) => {
    audio.setBpm(bpm);
});

app.ports.sidebarSetup.subscribe(({ waveformId, spectrumId }) => {
    setTimeout(() => {
        const waveform = document.getElementById(waveformId);
        const spectrum = document.getElementById(spectrumId);
        if (waveform && spectrum) {
            initSidebarRenderer({ waveform, spectrum });
        }
    }, 0);
});

app.ports.sidebarWheel.subscribe((deltaY) => {
    zoomBy(deltaY);
});

let score_editor = null;

app.ports.scorePanelSetup.subscribe(({ editorId, source }) => {
    setTimeout(() => {
        const editor = document.getElementById(editorId);
        if (editor && !score_editor) {
            score_editor = initScoreEditor({
                container: editor,
                initialValue: source,
                onChange: (code) => app.ports.scoreEditorChange.send(code),
            });
        }
    }, 0);
});

app.ports.scoreGraphUpdate.subscribe(({ source }) => {
    ScoreWasm.compile_score(source)
        .then((graph) => {
            app.ports.scoreGraphResult.send({ error: null, graph });
        })
        .catch((e) => {
            app.ports.scoreGraphResult.send({
                error: e instanceof Error ? e.message : String(e),
                graph: null,
            });
        });
});

export { setAnalysers };
