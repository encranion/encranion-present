To run an experiment you run the jar file and give it a few commands : 

```
java -jar encranion-present-assembly-0.1.jar <path to output> <path to experiment definition file> <COM port on the laptop that the ESU is connected to>
```

Let's say you have an experiment defintion file "expdeftest.txt"

To run an experiment you first create an output folder. This is where
the responses and timing of events (relative to the computer's time) 
will be written.

Let's say that this folder is located at "path/to/output/" and the ESU is connected to COM4

Then, to run the experiment, you run

```
java -jar encranion-present-assembly-0.1.jar path/to/output/ expdeftest.txt COM4
```

This will create files

path/to/output/responses.txt
path/to/output/timing.txt

responses.txt has a line for each response. The first item is the "unix epoch time" (search that on google if you need to)
and the second is the key code of the response. You can also google "key codes" if you need to. It should be easy to find a table.

timing.txt has a line for each event. The first item is the "unix epoch time" and the second item is the event.

You can use these two files along with the events in the EDF/bin file to match up the responses. timing.txt also allows you to
see if the ESU has any delay, which is possible. It is my understanding that the timing.txt file should be more accurate with respect
to when (relative to each other) the stimuli were displayed.





