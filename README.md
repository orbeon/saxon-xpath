# Scala port of the Saxon XPath processor

## Status

- 2020-08-12: sources compiles but `test` produces runtime errors. 

## Building and running locally

Run `sbt`:

```
sbt
```

In `sbt`:

```
> test
```

## Version

This version is up to date as of Saxon 10.1 (2020-05-14).

Saxon HE releases:

- Saxon 10.2: 2020-08-26
- Saxon 10.1: 2020-05-14
- Saxon 10.0: 2020-03-16

## Patches to port

- [ ] https://github.com/orbeon/saxon-he/commit/bc72ba742f6890c2df7ad00dea32f51e7599ddf2#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/525b0c58efc5f97d09082624bc3a6b2f7787bdeb#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/c89e30f232e5df2f4861a655ee5946f0607062d2#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/2de707fd5595a9745102f906d0780d99d692d610#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/f98065b9c3761d997ccb70f6cebab88baafc16a5#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/fe77a8762501f6b29a7e6d3ab6fa56768b923f99#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/1ac7b55e4a1fac22b3b1fefe67f8ff64aae07ca3#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/28964b256cda4d85a82c0495990041d3c93cf7d7#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/0573593a9ed1e6924040fb7d20f05f6471a21b20#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/6d49c596a52dbb73ea028d27278d3a7885e2d2b4#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/4cd28f0b17cc6b66e980baceb7c61ea6a30e57fb#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/561f17c9c04918c34d4e5a9824a363cd1cd3742a#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/a35007da4665e0607e2cc3872e64fdf8e71bc0fe#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/889c61180251c7d4a2e3642214f6fc95b1d821a5#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/b8adb110ace2ed85f2d052fd21545b8fefcf238e#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/35ff3033ed533955b8983cf872e0e55416da50c6#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/d0bfc0b879ab6073e02a6335f69a682c4ca04a6a#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/7b88e6feb55af1a8a9629fd4b18077d4efde397f#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/1decc60d929ebce49b8d7b33a8c55eaf2956eb38#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/fb7d392153c35a1e4a22d9a10cafd24799486f45#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/c43c2bf6cacbcc2102828ab77c153fe938910373#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/6384a774e13630b17a90cd4712098a7596919115#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/0f24f60db6f0b9c10ccbc8a145b641e8eac3cb95#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/50b08605e341318b36244e2c4957c67985cc8d7d#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/c10300c0a08488e8a8fa2a86e86f03b5e8ad8f1e#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/bcc8b5b7b737593a16284bb22ef14c1480985e63#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/2f6fecc25803ffa6dd94ab0f4ff764132f37b462#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/a9de9185c9089997c153a99be1114a70138cac4d#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/07a3ff7288d742bd5d1aa4b43446f532f224435c#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/7b147883d940deb8611d5bb6a3bfdf53dc23086c#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/0d7e59586c9dd0ecbb3d656bfe4ecc815fb044a9#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/1184db5d5b5d7b46fd67754ba2b6fffd61734738#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/155253988cd67f7227ee6d0341c7ced2506df6b2#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/b0483760b7251d16043473952a099d4f20b51a9b#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/a473fb6b7d89dc68d160231d527bf2e9f89c02b4#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/5e6d14350404201b7d3cda37ef3bddbfa23bf2a6#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/3568933065f17a914cac21ad13ff543dca3445c6#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/237a738bb7643fa538dd3675fee78b75343c2b56#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/00cf3217a948153559bedf3792b9ffc8e8f48511#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/33db9c753de54c2802e978b316bbc520a274f488#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/a8557f9fa92abfb6deef5bc8e073a313d7825625#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/78829cd08677a7c407e8e55526fae3556a57d67d#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/60b49097c3799145c39b7c0077167711b79745db#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/a82342957d16a5906cb79d9a9c1115b8baea9b09#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/6586d55bb7f382fda04e7aff050e3f6f22b39ccf#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/5c151903c811099c30fe237a43b86545cc19d3d4#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/e31d586ea0a46d7cc460045d7638c9eef0824469#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/8126e87970c965b77c0eafda54a9ceb2b389a7a7#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/7c6a6d9e48dfee98d1a0f84d3f478cbb5e6dc4e5#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/f9eb3a454962db321f59bde66e686df5229269e9#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/1173239589a179b313c743b12c5a7fa15cbfef1a#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/89a50aef37223761abe3c7f308bd9cf267eae416#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/827332bf76a693d7577f1169c9cd119731efe446#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/9ce46765e8016fdd9bb22247dca556562f218552#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/9ce46765e8016fdd9bb22247dca556562f218552#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/a2490f661f0e7da95b80ed007fbc4e1bf17606a7#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/4791a9e118e13d23b9500833a0a61249e1348235#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/e90fc9e5041dc7515291449baf0e11cf48fc9e75#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/9442c6ccc008a662779ea0ac03c47a7a5c8fb9f6#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/7559a5ce23dd4f23bb320fa5342d12d2ef75d53b#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/d2b458c0d9b0430bdee48f537375160c44340cc0#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/e5cb4f89b97633000285987b48638a53c6a81b51#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/5ee5e939fce6fa40f4f867becd4b3cc0a55d1167#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/ac9e45960b9e3256c75d5e585dfc367d312c940b#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
- [ ] https://github.com/orbeon/saxon-he/commit/7148a768c7a342e9f7e5e2371fdd70cb9c8093bd#diff-b5c820ca9934b965a40403efd2a6abe96683b68ed63baee7e50c25d4efddb12b
