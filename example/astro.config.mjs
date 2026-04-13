// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import starlightFSharpDoc from 'starlight-fsharp-doc';
import { fileURLToPath } from 'node:url';
import { resolve, dirname } from 'node:path';

const __dirname = dirname(fileURLToPath(import.meta.url));

// https://astro.build/config
export default defineConfig({
	integrations: [
		starlight({
			title: 'My Docs',
			social: [{ icon: 'github', label: 'GitHub', href: 'https://github.com/withastro/starlight' }],
			plugins: [
				starlightFSharpDoc({
					assemblies: [
						resolve(__dirname, '../tests/Reference/bin/Debug/net10.0/Reference.dll'),
					],
					output: 'api',
					sidebar: { label: 'API Reference' },
					command: [
						'dotnet',
						'run',
						'--project',
						resolve(__dirname, '../packages/FSharp.Oracle'),
						'--',
					],
				}),
			],
			sidebar: [
				{
					label: 'Guides',
					items: [
						// Each item here is one entry in the navigation menu.
						{ label: 'Example Guide', slug: 'guides/example' },
					],
				},
				{
					label: 'Reference',
					autogenerate: { directory: 'reference' },
				}
			],
		}),
	],
});
