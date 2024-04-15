## About

This repository contains an example Shiny application used as a companion the the talk _Taming Reactivity in Shiny_ given at [Appsilon's ShinyConf 2024](https://www.shinyconf.com/).
The talk is about state management in Shiny and slowing down reactivity to give you more control.
There are two branches that illustrate the ideas given in the talk.

The first is `conventional-shiny.` This branch implements the app in a traditional Shiny approach to state management.
That is, using reactive values (`reactiveVal`, `reactive`, etc.).

The second branch is called `refactored-state-management.` This branch implements the state management paradigm proposed in the talk.
It uses `R6` classes to store state, along with any logic needed to modify state.
It also uses the [`gargoyle`](https://github.com/ColinFay/gargoyle) package to implement event management, which helps to slow down reactivity.

## The data

The data used for this app is publicly available data pulled from the [MLS Fantasy site](https://fantasy.mlssoccer.com/).
By using this repository, you agree to abide by the MLS [Terms of Service](https://www.mlssoccer.com/legal/terms-of-service).

The data used for this application is for educational purposes only.
It is used solely to demonstrate how to implement the ideas given for the talk.
You may not use it for any other purpose than outlined in the Terms of Service.
