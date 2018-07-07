import { app, h } from "hyperapp"

const state = { greeting: 0 }

const greetings = ["Good evening!", "Good morning!", "Good afternoon!"]

const actions = {
  next: value => state => {
    return { greeting: (state.greeting + 1) % 3 }
  }
}

const view = (state, actions) => (
  <div onclick={actions.next}>
    <h2>{greetings[state.greeting]}</h2>
  </div>
)

const root = document.getElementById("app")

app(state, actions, view, root)
