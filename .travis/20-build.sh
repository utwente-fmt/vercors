#!/bin/bash
sbt compile "export compile:fullClasspath" | tee >(tail -n 1 > bin/.classpath)
