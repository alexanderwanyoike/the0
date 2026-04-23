// Shell runtime. Runs in the browser inside the `the0 dev --frontend` host
// page. Subscribes to the local /events WebSocket, maintains the same
// BotEventsContextValue shape the platform frontend exposes, and renders the
// user's bundle beneath it.
(function () {
  var R = window.React;
  var RD = window.ReactDOM;
  var Ctx = window.__THE0_EVENTS_CONTEXT__;

  function now() { return new Date(); }

  function utils(events) {
    return {
      metricsByType: function (type) { return events.filter(function (e) { return e.kind === 'metric' && e.metricType === type; }); },
      logs: function () { return events.filter(function (e) { return e.kind === 'log'; }); },
      lastMetric: function (type) {
        for (var i = events.length - 1; i >= 0; i--) {
          if (events[i].kind === 'metric' && events[i].metricType === type) return events[i];
        }
        return null;
      },
    };
  }

  function Provider(props) {
    var state = R.useState({ events: [], loading: true, error: null });
    var value = state[0]; var setValue = state[1];

    R.useEffect(function () {
      var ws = new WebSocket((location.protocol === 'https:' ? 'wss://' : 'ws://') + location.host + '/events');
      ws.onopen = function () { setValue(function (v) { return Object.assign({}, v, { loading: false }); }); };
      ws.onerror = function () { setValue(function (v) { return Object.assign({}, v, { error: 'websocket error' }); }); };
      ws.onmessage = function (msg) {
        try {
          var ev = JSON.parse(msg.data);
          if (ev.kind === 'restart') {
            setValue({ events: [], loading: false, error: null });
            return;
          }
          if (ev.kind === 'bot_stopped') return;
          setValue(function (v) {
            return { events: v.events.concat([{
              kind: ev.kind,
              type: ev.kind,
              data: ev.data ? JSON.parse(typeof ev.data === 'string' ? ev.data : JSON.stringify(ev.data)) : ev.raw,
              metricType: ev.metric_type,
              level: ev.level,
              timestamp: new Date(ev.timestamp),
            }]), loading: false, error: null };
          });
        } catch (e) { /* ignore malformed frames */ }
      };
      return function () { ws.close(); };
    }, []);

    var contextValue = {
      events: value.events,
      loading: value.loading,
      error: value.error,
      utils: utils(value.events),
      refresh: function () { /* no-op in dev — events are live */ },
      botId: window.__THE0_BOT_ID__ || 'dev',
    };

    return R.createElement(Ctx.Provider, { value: contextValue }, props.children);
  }

  // The Shell mounts the Provider and an inner div `the0-dev-bundle-root`.
  // User bundles MUST render into that inner div (via the global exposed
  // below) instead of #root — React context does not propagate across
  // separate ReactDOM.createRoot() trees, so a bundle that creates its own
  // root on #root would lose access to useThe0Events().
  function Shell() {
    return R.createElement(Provider, null, R.createElement('div', { id: 'the0-dev-bundle-root' }));
  }

  var bundleRootId = 'the0-dev-bundle-root';
  RD.createRoot(document.getElementById('root')).render(R.createElement(Shell));

  // Expose the bundle root id + element as globals. Bundles published for
  // `the0 dev` should render against window.__THE0_DEV_BUNDLE_ROOT__ (the
  // DOM node, already inside the Provider) instead of document.getElementById('root').
  // See docs/local-development/frontend.md for the exact pattern.
  window.__THE0_DEV_BUNDLE_ROOT_ID__ = bundleRootId;
  Object.defineProperty(window, '__THE0_DEV_BUNDLE_ROOT__', {
    get: function () { return document.getElementById(bundleRootId); },
  });

  // Dynamically import the bundle once the shell has mounted (so the
  // bundle root div actually exists in the DOM).
  queueMicrotask(function () {
    var script = document.createElement('script');
    script.type = 'module';
    script.src = window.__THE0_DEV_BUNDLE_URL__ || '/bundle.js';
    document.body.appendChild(script);
  });
})();
