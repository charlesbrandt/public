# Wine

Window Emulator on Linux

This looks like a good guide

https://www.fosslinux.com/26243/how-to-run-windows-apps-on-your-ubuntu-pc.htm

following along

https://ubuntuhandbook.org/index.php/2022/04/wine-ubuntu-2204-windows-apps/
https://linuxhint.com/install_wine_-ubuntu_20-24/

```
sudo dpkg --add-architecture i386
sudo apt update
sudo apt install wine
```

Hoping 64bit mode may be more supportive of newer applications? No, it's not necessary. `wine64` is already the newest

```
sudo apt install wine64
```



run winecfg in terminal to generate configuration file. A

```
winecfg
```

Applications -> Windows Version: 10
Graphics -> 168 DPI



Make a link to the .desktop file:

```
sudo ln -s /usr/share/doc/wine/examples/wine.desktop /usr/share/applications/```

Now you can run windows executable files. Download the application.

Right-click on an EXE file to run via “Wine Windows Program Loader” option


`.msi` files are Windows Installer packages. These files are used to install software on a Windows operating system. To install an `.msi` file using Wine, you can use the `msiexec` utility, which is part of the Wine suite. Here are the steps:

1. **Open a Terminal Window:** You will need to use the command line to run Wine commands.
2. **Locate the .msi File:** Ensure that you know the directory where the .msi file is located.
3. **Run the msiexec Command:** Use the following command to install the .msi package:
    

```
wine msiexec /i /path/to/your/installer.msi
```

Replace `/path/to/your/installer.msi` with the actual path to the .msi file you want to install.



After installing a Windows application via a `.msi` file in Wine, you can launch it using the following steps:

1. Access the Wine directory structure, which mimics a Windows installation, with a 'C:' drive located usually in your home directory under `~/.wine/drive_c/`.

2. Locate and run the executable file (.exe) for the application you have installed. This is often in `Program Files` or `Program Files (x86)` within the Wine `drive_c` directory. The executable is usually found in the main directory of the program or under a bin/ or exe/ subdirectory. `ls` is easier to use to navigate (better auto-complete):
    
```
ls ~/.wine/drive_c/Program\ Files/
wine ~/.wine/drive_c/Program\ Files/YourApplicationName
```


## Troubleshooting

I'm getting an error message: 

> DirectX failed to initialize. Please install the correct drivers for your video card! Error initializing text

[ChatGPT-web](https://niek.github.io/chatgpt-web/#/chat/9)

**Install Wine Libraries:** Make sure that you have the necessary libraries for DirectX support in Wine. You can install them using `winetricks`, a helper script for Wine, to install various redistributables.To do this, you may first need to install `winetricks`:

```
sudo apt-get install winetricks
```

[ChatGPT-web](https://niek.github.io/chatgpt-web/#/chat/9)

**Install DirectX:** With winetricks you can install various versions of DirectX:

```
winetricks d3dx9
```

This command will install the DirectX 9 libraries, which are often required for games and applications that depend on this version of DirectX.If you need a more comprehensive DirectX installation:

`winetricks directx9`
