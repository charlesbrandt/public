# Visual Studio Code

download and install from site:
https://code.visualstudio.com/download

add to favorites

launch

Disable telemetry
## How to disable telemetry reporting

https://code.visualstudio.com/docs/supporting/faq#_how-to-disable-telemetry-reporting

VS Code collects usage data and sends it to Microsoft to help improve our products and services. Read our privacy statement to learn more.

If you don't wish to send usage data to Microsoft, you can set the telemetry.enableTelemetry setting to false.

From File > Preferences > Settings (macOS: Code > Preferences > Settings), search for telemetry.enableTelemetry and uncheck the setting. This will silence all telemetry events from VS Code going forward. Telemetry information may have been collected and sent up until the point when you disable the setting.

If you use the JSON editor for your settings, add the following line:

    "telemetry.enableTelemetry": false
You can inspect telemetry events in the Output panel by setting the log level to Trace using Developer: Set Log Level from the Command Palette.

Important Notice: VS Code gives you the option to install Microsoft and third party extensions. These extensions may be collecting their own usage data and are not controlled by the telemetry.enableTelemetry setting. Consult the specific extension's documentation to learn about its telemetry reporting.

### How to disable crash reporting
VS Code collects data about any crashes that occur and sends it to Microsoft to help improve our products and services. Read our privacy statement to learn more.

If you don't wish to send crash data to Microsoft, you can set the telemetry.enableCrashReporter setting to false.

From File > Preferences > Settings (macOS: Code > Preferences > Settings), search for telemetry.enableCrashReporter and uncheck the setting.

If you use the JSON editor for your settings, add the following line:

    "telemetry.enableCrashReporter": false
Important Notice: This option requires a restart of VS Code to take effect.
