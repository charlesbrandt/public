# Checking the S.M.A.R.T status of a drive on linux

## skdump
d
```
sudo apt-get install gnome-disk-utility
```

The commandline version of the libatasmart library used by Gnome Disks is called skdump part of the libatasmart-bin package which is not installed by default. Install by using:

```
sudo apt-get install libatasmart-bin
```

Next run the following command to see the SMART information by running the following command in the terminal (replacing /dev/sda by the path to your drive):

```
sudo skdump /dev/sda
```

## smartctl

Back in [2014.03.26 10:26:06], this looks like a good tool:

```
sudo apt-get install smartmontools

sudo smartctl -i /dev/sda
```


If this command fails, you may need to let smartctl know what type of hard drive interface you’re using:

```
sudo smartctl -d TYPE -i /dev/sda
```

where TYPE is usually one of ata, scsi, or sat (for serial ata).


Now that smartctl can access the drive, let’s turn on some features. Run the following command:

```
sudo smartctl -s on -o on -S on /dev/sda
```

-s on: This turns on S.M.A.R.T. support or does nothing if it’s already enabled.
-o on: This turns on offline data collection. Offline data collection periodically updates certain S.M.A.R.T. attributes. Theoretically this could have a performance impact. However, from the smartctl man page:
Normally, the disk will suspend offline testing while disk accesses are taking place, and then automatically resume it when the disk would otherwise be idle, so  in  practice  it has little effect.

-S on: This enables “autosave of device vendor-specific Attributes”.


Next, let’s check the overall health:

```
sudo smartctl -H /dev/sda
```

This command should return:

```
=== START OF READ SMART DATA SECTION ===
SMART overall-health self-assessment test result: PASSED
```

If it doesn’t return PASSED, you should immediately backup all your data. Your hard drive is probably failing.





Make sure that the drive supports self-tests and see time estimates for each test:

```
sudo smartctl -c /dev/sda
```

Make sure “Self-test supported” appears in the “Offline data collection capabilities” section. Also, look for output similar to:

Short self-test routine
recommended polling time:        (   2) minutes.
Extended self-test routine
recommended polling time:        ( 127) minutes.
These are rough estimates of how long the short and long self-test’s will take respectively. Let’s run the short test:

```
sudo smartctl -t short /dev/sda
```

The time it takes to run varies by hardware. You can run:

```
sudo smartctl -l selftest /dev/sda
```

to check results. Unfortunately, there’s no way to check progress, so just keep running that command until the results show up. A successful run will look like:

```
=== START OF READ SMART DATA SECTION ===
SMART Self-test log structure revision number 1
Num  Test_Description    Status                  Remaining  LifeTime(hours)  LBA_of_first_error
# 1  Short offline       Completed without error       00%     21472         -
```

Now, do the same for the long self-test:

```
sudo smartctl -t long /dev/sda
```

The long test can take a significant amount of time. You might want to run it overnight and check for the results in the morning. If either test fails, you should immediately backup all your data and read the last section of this guide.



Via:
http://blog.shadypixel.com/monitoring-hard-drive-health-on-linux-with-smartmontools/
