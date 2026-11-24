import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

// Content root — the directory VitePress is pointed at (this file's parent).
const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..')

// Top-level sections that appear in the nav and get a generated sidebar.
const SECTIONS = ['code', 'documentation', 'pi', 'system', 'web']

const SKIP_DIRS = new Set(['.vitepress', '.git', '.obsidian', 'node_modules', '.emacs.d'])

// Prefer a file's first `# Heading` as its label; fall back to a tidied filename.
function titleOf(file, slug) {
  try {
    const head = fs.readFileSync(file, 'utf-8').slice(0, 4096)
    const body = head.replace(/^---\r?\n[\s\S]*?\r?\n---\r?\n/, '')
    const m = body.match(/^#\s+(.+)$/m)
    if (m) return m[1].trim()
  } catch {
    // unreadable file — fall through to the slug
  }
  return prettify(slug)
}

function prettify(slug) {
  return slug.replace(/[-_]/g, ' ').replace(/\b\w/g, (c) => c.toUpperCase())
}

// Walk one directory into VitePress sidebar items. Directories become collapsed
// groups; `index.md` becomes the group's own link rather than a child entry.
function walk(dir, urlBase) {
  let entries
  try {
    entries = fs.readdirSync(dir, { withFileTypes: true })
  } catch {
    return []
  }

  const groups = []
  const pages = []

  for (const entry of entries.sort((a, b) => a.name.localeCompare(b.name))) {
    if (entry.name.startsWith('.') || SKIP_DIRS.has(entry.name)) continue
    const full = path.join(dir, entry.name)

    if (entry.isDirectory()) {
      const items = walk(full, `${urlBase}${entry.name}/`)
      if (!items.length) continue
      const indexFile = path.join(full, 'index.md')
      const hasIndex = fs.existsSync(indexFile)
      const group = {
        text: hasIndex ? titleOf(indexFile, entry.name) : prettify(entry.name),
        collapsed: true,
        items
      }
      if (hasIndex) group.link = `${urlBase}${entry.name}/`
      groups.push(group)
    } else if (entry.name.endsWith('.md') && entry.name !== 'index.md') {
      const slug = entry.name.replace(/\.md$/, '')
      pages.push({ text: titleOf(full, slug), link: `${urlBase}${slug}` })
    }
  }

  // Pages before subsections: the leaf notes are what you usually want first.
  return [...pages, ...groups]
}

function sidebar() {
  const out = {}
  for (const section of SECTIONS) {
    const dir = path.join(ROOT, section)
    if (!fs.existsSync(dir)) continue
    const indexFile = path.join(dir, 'index.md')
    const hasIndex = fs.existsSync(indexFile)
    const group = {
      text: hasIndex ? titleOf(indexFile, section) : prettify(section),
      items: walk(dir, `/${section}/`)
    }
    if (hasIndex) group.link = `/${section}/`
    out[`/${section}/`] = [group]
  }
  return out
}

export default {
  title: 'Notes',
  description:
    'Still learning. Topics of interest. Writing, Code Development, Computer Systems, The Web, Design',
  // base: '/charles-brandt/',
  themeConfig: {
    nav: [
      { text: 'Code', link: '/code/' },
      { text: 'System', link: '/system/' },
      { text: 'Web', link: '/web/' },
      { text: 'Documentation', link: '/documentation/' },
      { text: 'Pi', link: '/pi/' }
    ],
    // Generated at build time by walking the content directories above — no
    // plugin dependency, so it works with the wrapper project's bare
    // vitepress + vue install (gitlab.com/charlesbrandt/charles-brandt).
    sidebar: sidebar(),
    outline: [2, 3]
  }
  // no effect in vitepress currently
  //dest: "./public"
}
