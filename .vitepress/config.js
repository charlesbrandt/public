// import { getSidebar } from 'vitepress-plugin-auto-sidebar'

export default {
  title: 'Notes',
  description: 'Still learning. Topics of interest. Writing, Code Development, Computer Systems, The Web, Design',
  // base: '/charles-brandt/',
  themeConfig: {
    nav: [
      { text: 'Code', link: '/code/' },
      { text: 'System', link: '/system/' },
      { text: 'Web', link: '/web/' },
      { text: 'Documentation', link: '/documentation/' },
      { text: 'Pi', link: '/pi/' },
    //  { text: '', link: '/' },

    ],
    //sidebar: [
    //  { text: 'Sidebar Test ', link: '/' },
    //  { text: '', link: '/' },
    //]
    // sidebar: getSidebar({ contentRoot: '/', contentDirs: ['code', 'documentation', 'pi', 'system', 'web'], collapsible: true, collapsed: true })
  }
  // no effect in vitepress currently
  //dest: "./public"
}
