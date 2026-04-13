namespace Starlight.FSharp.RenderImpl

type TocEntry =
    {
        Depth: int
        Slug: string
        Text: string
    }

type RenderedPage =
    {
        Title: string
        TemplateBody: string
        TocEntries: TocEntry list
    }
