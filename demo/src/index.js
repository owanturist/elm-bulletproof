import { Elm } from './Stories.elm';
import * as serviceWorker from './serviceWorker';


const SETTINGS_KEY = 'bf_settings'

const { ports } = Elm.Stories.init({
    flags: localStorage.getItem(SETTINGS_KEY)
});

ports.save_settings.subscribe(settings => localStorage.setItem(SETTINGS_KEY, settings))

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
