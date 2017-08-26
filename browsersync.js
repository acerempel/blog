/**
 * Require Browsersync
 */
var bs = require('browser-sync').create();

bs.init({
    ghostMode: false,
    open: false,
    port: 8000,
    ui: { port: 8001 },
    server: false,
    files: ["_site/styles/*.css","_site/*.html","_site/posts/*.html"],
    reloadDebounce: 500,
    browser: "safari",
    notify: false,
    plugins: [
        {
            module: "bs-html-injector",
            options: {
                files: ["_site/*.html","_site/posts/*.html"]
            }
        }
    ]
});
