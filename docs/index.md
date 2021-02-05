# Edinburgh Prelude
## NOTE
This is where students will be able to go to download and install the alternative Haskell Prelude for use by default. It is a **skeleton** only and is not intended for use until this note is removed. - 5 Feb 2021

### Stack [Installation/Home](https://docs.haskellstack.org/en/stable/README/) (UNTESTED)

### EPrelude installation, general: (UNTESTED)
1. Install stack as above.
2. Run `stack install [Package-Name-Undefined]`
3. At the top of files you wish to use this for, include the following pragma: `{-# LANGUAGE NoImplicitPrelude #-}`
4. Add `import EPrelude` to the import list of modules within that file. All Prelude typeclasses and methods aside from those explicitly removed or modified are now available.

### EPrelude installation, stack project/dependencies system: ([source](https://docs.haskellstack.org/en/stable/GUIDE/)) (UNTESTED)
1. Install stack as above.
2. Create a new project with `stack new`.
3. Open the newly created `package.yaml` file.
4. Under 'dependencies' add a line with `-e-prelude`.
5. Under 'default-extensions' add a line with `NoImplicitPrelude`. All Prelude typeclasses and methods aside from those explicitly removed or modified are now available within this stack project.
