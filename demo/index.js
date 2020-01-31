import { Elm } from './src/Stories.elm';

const SETTINGS_KEY = 'bf_settings'

const { ports } = Elm.Stories.init({
    flags: localStorage.getItem(SETTINGS_KEY)
});

ports.save_settings.subscribe(settings => localStorage.setItem(SETTINGS_KEY, settings))
