# Audio

## Amplitude JS

Playlist object -- ready to go

https://521dimensions.com/open-source/amplitudejs/docs/configuration/playlists.html  
Playlists - AmplitudeJS Documentation  
https://github.com/serversideup/amplitudejs  
serversideup/amplitudejs: AmplitudeJS: Open Source HTML5 Web Audio Library. Design your web audio player, the way you want. No dependencies required.  
https://duckduckgo.com/?t=ffab&q=amplitudejs+vue3&ia=web&atb=v343-1  
amplitudejs vue3 at DuckDuckGo  
https://github.com/serversideup/amplitudejs/issues/371  
VueJS Component to easily use AmplitudeJS within VueJS · Issue #371 · serversideup/amplitudejs  
https://opencollective.com/amplitudejs  
AmplitudeJS - Open Collective  

## Clapper 

https://github.com/Metrakit/clappr-comment-plugin  
GitHub - Metrakit/clappr-comment-plugin: A comments plugin for the Clappr player  
https://github.com/clappr/clappr  
GitHub - clappr/clappr: An extensible media player for the web.  
https://github.com/joaopaulovieira/clappr-queue-plugin  
GitHub - joaopaulovieira/clappr-queue-plugin: A queue plugin for Clappr Player to play videos in sequence.  


## Tone JS

https://tonejs.github.io/  
Tone.js  
https://tonejs.github.io/examples/stepSequencer  
Step Sequencer  
https://tonejs.github.io/examples/mic  
Microphone  

https://duckduckgo.com/?t=ffab&q=tone.js+record+microphone&ia=web&atb=v343-1  
tone.js record microphone at DuckDuckGo  
https://duckduckgo.com/?t=ffab&q=tone.js+record&ia=web&atb=v343-1  
tone.js record at DuckDuckGo  
https://tonejs.github.io/docs/14.7.39/Recorder  
Recorder  
https://tonejs.github.io/docs/14.7.39/Player  
Player  




## Record

``` vue
<template>
  <div>
    <button @click="toggleRecording">
      {{ isRecording ? 'Stop Recording' : 'Start Recording' }}
    </button>
    <div id="waveform"></div>
  </div>
</template>

<script setup>
import { ref, onMounted } from 'vue'
import dayjs from 'dayjs'
import WaveSurfer from 'wavesurfer.js'

import { useSupabaseStore } from '@/stores/supabaseStore'

const supabase = useSupabaseStore().client

let wavesurfer

function initWaveform() {
  wavesurfer = WaveSurfer.create({
    container: '#waveform', // id of the div where the waveform should be rendered
    waveColor: 'violet',
    progressColor: 'purple',
  })
}

const isRecording = ref(false)
let recorder
let audioChunks = []

async function toggleRecording() {
  if (!isRecording.value) {
    startRecording()
  } else {
    stopRecording()
  }
}

let mediaRecorder

async function startRecording() {
  const stream = await navigator.mediaDevices.getUserMedia({ audio: true })

  mediaRecorder = new MediaRecorder(stream)
  mediaRecorder.ondataavailable = (event) => {
    if (event.data.size > 0) {
      audioChunks.push(event.data)
      wavesurfer.loadBlob(event.data)
    }
  }
  // This event will trigger when the recorder is stopped
  mediaRecorder.onstop = () => {
    const completeBlob = new Blob(audioChunks, { type: 'audio/wav' })
    sendToServer(completeBlob)
  }

  mediaRecorder.start(30000) // 30 seconds chunk
  isRecording.value = true
}

async function stopRecording() {
  mediaRecorder.stop()
  isRecording.value = false
}

function onDataAvailable(e) {
  console.log('onDataAvailable called')
  audioChunks.push(e.data)
  sendToServer(e.data)
}

async function sendToServer(data) {
  const file = new Blob([data], { type: 'audio/wav' }) // adjust MIME type if needed
  const formattedDate = dayjs().format('YYYY-MM-DD-HHmmss')
  const fileName = `time/${formattedDate}.wav` // a unique filename based on current timestamp

  console.log('Saving recording:', fileName)

  const { error: uploadError } = await supabase.storage.from('recordings').upload(fileName, file)

  if (uploadError) {
    console.error('Failed to upload audio to storage:', uploadError)
    return
  }

  // If you wish, you can then store the fileName or URL to the database
  // const { data: audioData, error: dbError } = await supabase
  //   .from('recordings')
  //   .insert([{ file_name: fileName }]);
  // if (dbError) {
  //   console.error("Failed to save audio reference to database:", dbError);
  // }
}

// Use onMounted to call the async function
onMounted(() => {
  initWaveform()
})
</script>

```

https://github.com/topics/audio-recorder  
audio-recorder · GitHub Topics  

https://duckduckgo.com/?t=ffab&q=html5+microphone+input+&ia=web&atb=v343-1  
html5 microphone input at DuckDuckGo  
https://subinsb.com/html5-record-mic-voice/  
Record, Play, Download Microphone Sound With HTML5 - Subin's Blog  
https://github.com/higuma/web-audio-recorder-js  
higuma/web-audio-recorder-js: .wav/.ogg/.mp3 recorder with Web Audio API  
https://github.com/Tonejs/Tone.js  
Tonejs/Tone.js: A Web Audio framework for making interactive music in the browser.  

https://github.com/Canardoux/flutter_sound  
Canardoux/flutter_sound: Flutter plugin for sound. Audio recorder and player.  


## Web Audio

https://github.com/topics/web-audio  
web-audio · GitHub Topics  

https://duckduckgo.com/?t=ffab&q=web+audio&ia=web&atb=v343-1  
web audio at DuckDuckGo  
https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API  
Web Audio API - Web APIs | MDN  
https://duckduckgo.com/?t=ffab&q=How+do+I+record+audio+in+a+web+browser+using+javascript+and+the+web+audio+API%3F&ia=web&atb=v343-1  
How do I record audio in a web browser using javascript and the web audio API? at DuckDuckGo  
https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API/Using_Web_Audio_API  
Using the Web Audio API - Web APIs | MDN  
https://github.com/mdn/webaudio-examples/tree/main/spatialization  
webaudio-examples/spatialization at main · mdn/webaudio-examples · GitHub  
https://github.com/mdn/webaudio-examples/blob/main/spatialization/index.html  
webaudio-examples/spatialization/index.html at main · mdn/webaudio-examples · GitHub  
https://github.com/mdn/webaudio-examples/blob/main/voice-change-o-matic/scripts/app.js  
webaudio-examples/voice-change-o-matic/scripts/app.js at main · mdn/webaudio-examples · GitHub  
https://mdn.github.io/webaudio-examples/  
webaudio-examples | Code examples that accompany the MDN Web Docs pages relating to Web Audio.  
https://mdn.github.io/webaudio-examples/audio-buffer/  
Web Audio API: AudioBuffer  
https://mdn.github.io/webaudio-examples/voice-change-o-matic/  
Voice-change-O-matic  
https://github.com/mdn/webaudio-examples/blob/main/voice-change-o-matic/index.html  
webaudio-examples/voice-change-o-matic/index.html at main · mdn/webaudio-examples · GitHub  

## Waveform display

Remember to consult the documentation of the chosen library for any nuances, additional configuration, or features. Also, take into consideration any additional performance implications, especially if you're processing large audio files or dealing with real-time data.

### Wavesurfer

1. **wavesurfer.js**: 
    - **Description**: This is one of the most popular and feature-rich libraries for generating waveforms and spectrograms. It's easy to use, customizable, and comes with a variety of plugins.
    - **Size**: The core library is lightweight, but if you include plugins, the size can increase.


https://github.com/wavesurfer-js/wavesurfer.js  
wavesurfer-js/wavesurfer.js: Navigable waveform built on Web Audio and Canvas  
https://wavesurfer-js.org/projects/  
wavesurfer.js  

  

#### Integration:

For demonstration, here's a basic way to integrate `wavesurfer.js`:

1. Install via npm:
    ```bash
    npm install wavesurfer.js
    ```

2. Integrate into your component:

```javascript
import WaveSurfer from 'wavesurfer.js';

let wavesurfer;

function initWaveform() {
  wavesurfer = WaveSurfer.create({
      container: '#waveform', // id of the div where the waveform should be rendered
      waveColor: 'violet',
      progressColor: 'purple'
  });
}

// After recording or when you want to load the audio blob into the waveform
function loadIntoWaveform(blob) {
  wavesurfer.loadBlob(blob);
}
```

3. In your template, include a container for the waveform:

```html
<div id="waveform"></div>
```

4. After recording, or when you want to display a previously recorded audio, call `loadIntoWaveform` with the audio blob.



### Peaks

3. **BBC/audiowaveform**:
    - **Description**: Although not purely a frontend library (it has a backend component for generating the waveform data), the BBC's audiowaveform is worth mentioning. It's robust and was designed for broadcast applications.
    - **Size**: Varies depending on how you integrate it.
    - **URL**: [audiowaveform on GitHub](https://github.com/bbc/audiowaveform)


https://betterprogramming.pub/peaks-js-interact-with-audio-waveforms-b7cb5bd3939a  
Peaks.js — Interact With Audio Waveforms | by Trevor-Indrek Lasn | Better Programming  
https://github.com/bbc/peaks.js  
GitHub - bbc/peaks.js: JavaScript UI component for interacting with audio waveforms  
https://waveform.prototyping.bbc.co.uk/  
BBC Research and Development: Audio Waveforms  



## Editors

https://wilsonlmh.github.io/fiveLoadSub/  
fiveLoadSub  
https://rawgit.com/wilsonlmh/fiveLoadSub/master/index.html#convertDialogTab_subtitle  
fiveLoadSub  
https://github.com/kernow-ged/loopslicer  
kernow-ged/loopslicer: A web app for slicing and dicing audio files  
https://kernow.me/loopslicer/  
kernow.me::loopslicer  
https://www.linuxsampler.org/screenshots.html  
The Linux Sampler Project  
https://github.com/naomiaro/waveform-playlist  
naomiaro/waveform-playlist: Multitrack Web Audio editor and player with canvas waveform preview. Set cues, fades and shift multiple tracks in time. Record audio tracks or provide audio annotations. Export your mix to AudioBuffer or WAV! Add effects from Tone.js. Project inspired by Audacity.  
http://naomiaro.github.io/waveform-playlist/web-audio-editor.html  
Full Waveform Editor  
https://naomiaro.github.io/waveform-playlist/record.html  
Record An Audio Track  
https://www.sourcefabric.org/software/airtime  
Sourcefabric | Airtime  
https://leanpub.com/masteringtonejs  
Mastering Tone.js by Naomi Aro [Leanpub PDF/iPad/Kindle]  
https://github.com/collab-project/videojs-record  
collab-project/videojs-record: video.js plugin for recording audio/video/image files  



https://duckduckgo.com/?q=web+audio+recorder+editor+open+source&t=ffab&ia=web  
web audio recorder editor open source at DuckDuckGo  
https://duckduckgo.com/?q=realtime+web+audio+microphone+effects&t=ffab&ia=web  
realtime web audio microphone effects at DuckDuckGo  
https://github.com/topics/audio-recorder?l=javascript  
audio-recorder · GitHub Topics  
https://github.com/topics/web-audio  
web-audio · GitHub Topics  


https://audiomass.co/index-cache.html  
AudioMass - Audio Editor  
https://audiomass.co/  
AudioMass - Audio Editor  
https://duckduckgo.com/?t=ffab&q=audiomass+record+stereo+audio&ia=web  
audiomass record stereo audio at DuckDuckGo  
https://github.com/pkalogiros/AudioMass/issues/65  
Can't Record Browser in Stereo (mac M1) via VB-Cable · Issue #65 · pkalogiros/AudioMass  
https://soundvisualiser.com/#/visualiser  
Online Microphone and Audio Visualizer  
https://github.com/Theodeus/tuna  
Theodeus/tuna: An audio effects library for the Web Audio API.  
https://github.com/goldfire/howler.js#group-playback  
goldfire/howler.js: Javascript audio library for the modern web.  
https://github.com/tonistiigi/audiosprite  
tonistiigi/audiosprite: Jukebox/Howler/CreateJS compatible audio sprite generator  
https://duckduckgo.com/?t=ffab&q=web+audio+looper+open+source&ia=web  
web audio looper open source at DuckDuckGo  
https://middleearmedia.com/labs/apps/web-audio-loop-mixer/  
Web Audio Loop Mixer | Middle Ear Media  
https://middleearmedia.com/webaudioloopmixer/  
Web Audio Loop Mixer by Middle Ear Media  


## Howler

Add an Audio Player with Vue 3 and JavaScript - The Web Dev  
https://duckduckgo.com/?t=ffab&q=howler.js&ia=web&atb=v343-1  
howler.js at DuckDuckGo  
https://howlerjs.com/  
howler.js - JavaScript audio library for the modern web  
https://github.com/goldfire/howler.js  
goldfire/howler.js: Javascript audio library for the modern web.  



## Audio Player Component

https://duckduckgo.com/?q=web+audio+vue3&t=ffab&ia=web&atb=v343-1  
web audio vue3 at DuckDuckGo  
https://vuejsexamples.com/a-beautiful-audio-player-component-for-vue3/  
A beautiful Audio Player Component for Vue3  
https://vuejsexamples.com/easy-to-create-custom-audio-player-components-for-vue/  
Easy to create custom audio player components for Vue  
https://github.com/forijk/vue-audio-better/blob/master/src/MiniAudio.vue  
vue-audio-better/src/MiniAudio.vue at master · forijk/vue-audio-better · GitHub  
https://github.com/forijk/vue-audio-better  
forijk/vue-audio-better: :stuck_out_tongue_winking_eye: :stuck_out_tongue_winking_eye: Easy to create custom audio player components for Vue. 一个有灵魂的进度条。 A progress bar with soul.  
https://goldfirestudios.com/  
GoldFire Studios - Indie Game Developer  
https://thewebdev.info/2021/01/21/add-an-audio-player-with-vue-3-and-javascript/  

