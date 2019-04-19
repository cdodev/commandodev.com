const App = require('../output/Galaxy');

// window.__appState = App.main(window.__appState || App.initialState)();
App.main();
module.hot.accept();
