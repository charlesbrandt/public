// start-here.spec.js created with Cypress
//
// Start writing your Cypress tests below!
// If you're unfamiliar with how Cypress works,
// check out the link below and learn how to write your first test:
// https://on.cypress.io/writing-first-test

describe('My First Test', () => {
  it('Does not do much!', () => {
    expect(true).to.equal(true)
  })
})

describe('My Second Test', () => {
  it('Goes to a search engine!', () => {
    cy.visit('https://google.com')
    cy.visit('https://duckduckgo.com')
    // aha! only one main site is allowed...
    // good to know
  })
})

