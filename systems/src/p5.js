const App = require('../output/Field');

window.__appState = App.main(window.__appState || App.initialState)();
module.hot.accept();
