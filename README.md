# Scala port of the Saxon XPath processor

## Status

- Cross-compiles and passes tests on **JVM** and **Scala.js** across **Scala 2.13** and **Scala 3**.

## Building and running locally

Run `sbt`:

```
sbt
```

In `sbt`, to run tests for the default Scala version:

```
> test
```

To run tests across all supported Scala versions (Scala 2.13 and Scala 3) on both JVM and Scala.js:

```
> +test
```

## Version

This version is up to date as of **Saxon-HE 10.9** (2023-02-16) for the XPath processor subset.

Saxon-HE 10.x releases integrated:

- Saxon 10.9: 2023-02-16
- Saxon 10.8: 2022-03-15
- Saxon 10.7: 2022-03-01
- Saxon 10.6: 2021-09-16
- Saxon 10.5: 2021-04-14
- Saxon 10.3: 2020-10-28
- Saxon 10.2: 2020-08-26
- Saxon 10.1: 2020-05-14
- Saxon 10.0: 2020-03-16
