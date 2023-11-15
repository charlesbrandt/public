# Pyaudio

http://people.csail.mit.edu/hubert/pyaudio/
PyAudio: PortAudio v19 Python Bindings


Even in a [virutalenv](virtualenv.md), `pyaudio` needs a few dependencies to be installed at the system. If you haven't already, be sure to get:

```
sudo apt-get install python-is-python3
sudo apt-get install python3-pip
```

```
sudo apt install portaudio19-dev
```

```
poetry add pyaudio
```

Example script

```python
"""PyAudio example: Record a few seconds of audio and save to a WAVE file."""

import pyaudio
import wave

CHUNK = 1024
FORMAT = pyaudio.paInt16
CHANNELS = 2
RATE = 44100
RECORD_SECONDS = 5
WAVE_OUTPUT_FILENAME = "output.wav"

p = pyaudio.PyAudio()

stream = p.open(format=FORMAT,
                channels=CHANNELS,
                rate=RATE,
                input=True,
                frames_per_buffer=CHUNK)

print("* recording")

frames = []

for i in range(0, int(RATE / CHUNK * RECORD_SECONDS)):
    data = stream.read(CHUNK)
    frames.append(data)

print("* done recording")

stream.stop_stream()
stream.close()
p.terminate()

wf = wave.open(WAVE_OUTPUT_FILENAME, 'wb')
wf.setnchannels(CHANNELS)
wf.setsampwidth(p.get_sample_size(FORMAT))
wf.setframerate(RATE)
wf.writeframes(b''.join(frames))
wf.close()
```



## Troubleshooting

I was seeing the following error

```
x86_64-linux-gnu-gcc -Wno-unused-result -Wsign-compare -DNDEBUG -g -fwrapv -O2 -Wall -g -fstack-protector-strong -Wformat -Werror=format-security -g -fwrapv -O2 -fPIC -I/usr/local/include -I/usr/include -I/tmp/tmp64rqtbrl/.venv/include -I/usr/include/python3.10 -c src/pyaudio/device_api.c -o build/temp.linux-x86_64-cpython-310/src/pyaudio/device_api.o
  src/pyaudio/device_api.c:9:10: fatal error: portaudio.h: No such file or directory
      9 | #include "portaudio.h"
        |          ^~~~~~~~~~~~~
  compilation terminated.
  error: command '/usr/bin/x86_64-linux-gnu-gcc' failed with exit code 1
```

fixed with: 

```
sudo apt install portaudio19-dev
```

These had no effect:

```
sudo apt-get install libportaudio2
sudo apt-get install libasound-dev
```


https://duckduckgo.com/?q=pyaudio&t=canonical&ia=about
pyaudio (Python) at DuckDuckGo
