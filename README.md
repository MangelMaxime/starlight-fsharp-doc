# FSharp.Oracle

> **Reads the binaries. Sees the truth. Writes the docs.** 🔮

FSharp.Oracle is a documentation generator for F# libraries. It inspects compiled assemblies using the F# Compiler Service (FCS) and generates rich API reference documentation as [Starlight](https://starlight.astro.build/) pages.

## Architecture

Two F# components bridged by a JSON intermediate representation:

- **Oracle** — `packages/Oracle/`
  - Uses FCS (F# Compiler Service) to inspect compiled assemblies
  - Extracts types, functions, members, and XML documentation
  - Emits a JSON IR

- **Starlight.FSharp.Doc** — `packages/Starlight.FSharp.Oracle/`
  - Starlight plugin written in F# and compiled via [Fable](https://fable.io/)
  - Reads the JSON IR and generates `.mdx` pages
  - Injects sidebar groups and CSS theme variables

## Installation

```bash
npm install starlight-fsharp-doc
```

You will also need the .NET Oracle tool alongside your Starlight site:

```bash
git clone https://github.com/your-org/starlight-fsharp-doc.git
cd packages/Oracle
```

## Usage

TODO 🚨

Add the plugin to your `astro.config.mjs`:

```js
import starlightFSharpDoc from 'starlight-fsharp-doc';
import { resolve } from 'node:path';

export default defineConfig({
  integrations: [
    starlight({
      plugins: [
        starlightFSharpDoc({
          // Paths to compiled F# assemblies
          assemblies: [
            resolve(__dirname, '../src/MyLibrary/bin/Debug/net10.0/MyLibrary.dll'),
          ],
          // Output base path for generated pages
          output: 'api',
          // Sidebar configuration
          sidebar: { label: 'API Reference' },
          // Command to run Oracle (optional — defaults shown)
          command: [
            'dotnet',
            'run',
            '--project',
            resolve(__dirname, '../packages/Oracle'),
            '--',
          ],
        }),
      ],
    }),
  ],
});
```

The plugin runs during `dev`, `build`, and `sync`. It skips extraction on `preview`.

## Development

TODO 🚨

## License

MIT
