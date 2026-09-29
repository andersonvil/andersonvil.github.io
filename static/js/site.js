(() => {
  const root = document.documentElement;
  const themeButton = document.querySelector('.theme-toggle');

  const currentTheme = () => root.dataset.theme === 'dark' ? 'dark' : 'light';
  const updateThemeControl = () => {
    if (!themeButton) return;
    const next = currentTheme() === 'dark' ? 'light' : 'dark';
    const label = `Switch to ${next} theme`;
    themeButton.setAttribute('aria-label', label);
    themeButton.setAttribute('title', label);
  };
  updateThemeControl();

  themeButton?.addEventListener('click', () => {
    const next = currentTheme() === 'dark' ? 'light' : 'dark';
    root.dataset.theme = next;
    root.style.colorScheme = next;
    try { localStorage.setItem('av-theme', next); } catch (_) { /* Preference remains for this page. */ }
    updateThemeControl();
  });

  const menuButton = document.querySelector('.menu-toggle');
  const mobileMenu = document.querySelector('#mobile-menu');
  const closeMenu = (restoreFocus = false) => {
    if (!menuButton || !mobileMenu) return;
    mobileMenu.hidden = true;
    menuButton.setAttribute('aria-expanded', 'false');
    menuButton.textContent = 'Menu';
    if (restoreFocus) menuButton.focus();
  };
  const openMenu = () => {
    if (!menuButton || !mobileMenu) return;
    mobileMenu.hidden = false;
    menuButton.setAttribute('aria-expanded', 'true');
    menuButton.textContent = 'Close';
    mobileMenu.querySelector('a')?.focus();
  };
  menuButton?.addEventListener('click', () => {
    if (menuButton.getAttribute('aria-expanded') === 'true') closeMenu(true);
    else openMenu();
  });
  mobileMenu?.querySelectorAll('a').forEach((link) => link.addEventListener('click', () => closeMenu(false)));
  document.addEventListener('keydown', (event) => {
    if (event.key === 'Escape' && menuButton?.getAttribute('aria-expanded') === 'true') closeMenu(true);
    if (event.key === 'Tab' && menuButton?.getAttribute('aria-expanded') === 'true' && mobileMenu) {
      const links = [...mobileMenu.querySelectorAll('a')];
      const first = links[0];
      const last = links[links.length - 1];
      if (event.shiftKey && document.activeElement === first) { event.preventDefault(); menuButton.focus(); }
      else if (!event.shiftKey && document.activeElement === last) { event.preventDefault(); menuButton.focus(); }
    }
  });
  window.addEventListener('resize', () => { if (window.innerWidth > 960) closeMenu(false); });

  const contextualRoutes = {
    home: { path: '/', label: 'Home' },
    contact: { path: '/contact/', label: 'Contact' },
    research: { path: '/research/', label: 'Research' },
    teaching: { path: '/teaching/', label: 'Teaching' },
    'code-data': { path: '/code-data/', label: 'Code & Data' },
    insights: { path: '/insights/', label: 'Insights' },
    'beyond-economics': { path: '/beyond-economics/', label: 'Beyond Economics' }
  };
  const contextualParams = new URLSearchParams(window.location.search);
  const contextualOriginKey = contextualParams.get('from');
  const contextualOrigin = contextualRoutes[contextualOriginKey];
  const contextualHost = document.querySelector('[data-contextual-return-host]');
  const currentTopRoute = Object.values(contextualRoutes).find((route) => route.path === window.location.pathname);
  if (contextualHost && contextualOrigin && currentTopRoute && contextualOrigin.path !== currentTopRoute.path) {
    const link = document.createElement('a');
    link.className = 'contextual-return-link';
    link.href = contextualOrigin.path;
    link.textContent = `← Back to ${contextualOrigin.label}`;
    contextualHost.replaceChildren(link);
    contextualHost.hidden = false;
  }
  if (contextualOrigin) {
    const activeSection = Object.values(contextualRoutes).find((route) => window.location.pathname === route.path || (route.path !== '/' && window.location.pathname.startsWith(route.path)));
    if (activeSection) {
      document.querySelectorAll(`a[href^="${activeSection.path}"]`).forEach((link) => {
        if (link.closest('.desktop-nav, .mobile-nav, .site-footer') || link.hasAttribute('download')) return;
        const url = new URL(link.href, window.location.origin);
        if (url.origin !== window.location.origin || !url.pathname.startsWith(activeSection.path)) return;
        url.searchParams.set('from', contextualOriginKey);
        link.href = `${url.pathname}${url.search}${url.hash}`;
      });
    }
  }

  document.querySelectorAll('[data-scrollspy]').forEach((toc) => {
    const links = [...toc.querySelectorAll('.paper-toc-links a[href^="#"]')];
    const sections = links.map((link) => document.getElementById(link.hash.slice(1))).filter(Boolean);
    if (!links.length || !sections.length) return;
    const reducedMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    let activeId = '';
    const setActive = (id) => {
      if (!id || id === activeId) return;
      activeId = id;
      links.forEach((link) => {
        if (link.hash === `#${id}`) link.setAttribute('aria-current', 'location');
        else link.removeAttribute('aria-current');
      });
    };
    const resolveActive = () => {
      const marker = Math.min(240, window.innerHeight * 0.3);
      let current = sections[0];
      sections.forEach((section) => { if (section.getBoundingClientRect().top <= marker) current = section; });
      setActive(current.id);
    };
    links.forEach((link) => link.addEventListener('click', (event) => {
      const target = document.getElementById(link.hash.slice(1));
      if (!target) return;
      event.preventDefault();
      target.scrollIntoView({ behavior: reducedMotion ? 'auto' : 'smooth', block: 'start' });
      history.replaceState(history.state, '', `${window.location.pathname}${window.location.search}${link.hash}`);
      setActive(target.id);
    }));
    const observer = new IntersectionObserver(resolveActive, { rootMargin: '-18% 0px -66% 0px', threshold: [0, 0.05, 0.5] });
    sections.forEach((section) => observer.observe(section));
    window.addEventListener('scroll', resolveActive, { passive: true });
    resolveActive();
  });

  document.querySelectorAll('.abstract-toggle').forEach((button) => {
    const panel = document.getElementById(button.getAttribute('aria-controls'));
    if (!panel) return;
    button.addEventListener('click', () => {
      const expanded = button.getAttribute('aria-expanded') === 'true';
      button.setAttribute('aria-expanded', String(!expanded));
      button.textContent = expanded ? 'View abstract' : 'Hide abstract';
      panel.hidden = expanded;
    });
  });

  document.querySelectorAll('[data-filter-root]').forEach((filterRoot) => {
    const buttons = [...filterRoot.querySelectorAll('[data-filter]')];
    const items = [...filterRoot.querySelectorAll('[data-filter-item]')];
    buttons.forEach((button) => button.addEventListener('click', () => {
      const filter = button.dataset.filter;
      buttons.forEach((candidate) => {
        const active = candidate === button;
        candidate.classList.toggle('active', active);
        candidate.setAttribute('aria-pressed', String(active));
      });
      items.forEach((item) => { item.hidden = filter !== 'all' && item.dataset.filterItem !== filter; });
    }));
  });

  let activePoemAudio = null;
  const allAudioElements = [...document.querySelectorAll('audio')];
  allAudioElements.forEach((audio) => audio.addEventListener('play', () => {
    allAudioElements.forEach((other) => { if (other !== audio && !other.paused) other.pause(); });
  }));
  const formatAudioTime = (seconds) => {
    if (!Number.isFinite(seconds)) return '0:00';
    const whole = Math.max(0, Math.floor(seconds));
    return `${Math.floor(whole / 60)}:${String(whole % 60).padStart(2, '0')}`;
  };
  document.querySelectorAll('[data-poem-audio]').forEach((player) => {
    const audio = player.querySelector('[data-audio-element]');
    const toggle = player.querySelector('.audio-play-toggle');
    const progress = player.querySelector('.audio-progress');
    const time = player.querySelector('.audio-time');
    const volume = player.querySelector('.audio-volume');
    const status = player.querySelector('.audio-status');
    if (!audio || !toggle || !progress || !time || !volume || !status) return;
    audio.volume = Number(volume.value);
    const renderTime = () => {
      progress.max = Number.isFinite(audio.duration) ? String(audio.duration) : '0';
      progress.value = String(audio.currentTime || 0);
      time.textContent = `${formatAudioTime(audio.currentTime)} / ${formatAudioTime(audio.duration)}`;
    };
    const renderPaused = (message = 'Paused.') => {
      toggle.textContent = 'Play music';
      toggle.setAttribute('aria-pressed', 'false');
      status.textContent = message;
    };
    toggle.addEventListener('click', async () => {
      if (!audio.paused) { audio.pause(); return; }
      if (activePoemAudio && activePoemAudio !== audio) activePoemAudio.pause();
      status.textContent = 'Loading music…';
      try { await audio.play(); }
      catch (_) { renderPaused('Music could not be played. Check the file or browser audio permissions.'); }
    });
    audio.addEventListener('play', () => {
      activePoemAudio = audio;
      toggle.textContent = 'Pause music';
      toggle.setAttribute('aria-pressed', 'true');
      status.textContent = `Playing ${player.dataset.trackTitle}.`;
    });
    audio.addEventListener('pause', () => { if (!audio.ended) renderPaused(); });
    audio.addEventListener('ended', () => { audio.currentTime = 0; renderTime(); renderPaused('Playback finished.'); });
    audio.addEventListener('loadedmetadata', renderTime);
    audio.addEventListener('timeupdate', renderTime);
    audio.addEventListener('waiting', () => { status.textContent = 'Loading music…'; });
    audio.addEventListener('error', () => renderPaused('Music could not be loaded.'));
    progress.addEventListener('input', () => { audio.currentTime = Number(progress.value); renderTime(); });
    volume.addEventListener('input', () => { audio.volume = Number(volume.value); });
    renderTime();
  });
  window.addEventListener('pagehide', () => { allAudioElements.forEach((audio) => audio.pause()); });

  const copyText = async (text) => {
    if (navigator.clipboard?.writeText) return navigator.clipboard.writeText(text);
    const area = document.createElement('textarea');
    area.value = text;
    area.setAttribute('readonly', '');
    area.style.position = 'fixed';
    area.style.opacity = '0';
    document.body.append(area);
    area.select();
    const copied = document.execCommand('copy');
    area.remove();
    if (!copied) throw new Error('Copy unavailable');
  };

  document.querySelectorAll('.copy-citation').forEach((button) => button.addEventListener('click', async () => {
    const status = button.closest('header')?.querySelector('.copy-status');
    try { await copyText(button.dataset.citation || ''); if (status) status.textContent = 'Preview citation copied.'; }
    catch (_) { if (status) status.textContent = 'Copying was unavailable. Select the citation text manually.'; }
  }));

  document.querySelectorAll('.copy-email').forEach((button) => button.addEventListener('click', async () => {
    const status = document.querySelector('.copy-email-status');
    try { await copyText(button.dataset.email || ''); if (status) status.textContent = 'Email address copied.'; }
    catch (_) { if (status) status.textContent = 'Copying was unavailable. Select the email address manually.'; }
  }));

  document.querySelectorAll('[data-meeting-demo]').forEach((demo) => {
    const topicButtons = [...demo.querySelectorAll('[data-meeting-topic]')];
    const slotButtons = [...demo.querySelectorAll('[data-meeting-slot]')];
    const zone = demo.querySelector('[data-meeting-timezone]');
    const review = demo.querySelector('[data-meeting-review]');
    const confirm = demo.querySelector('[data-meeting-confirm]');
    const status = demo.querySelector('.meeting-status');
    let topic = '';
    let slot = '';
    try { zone.textContent = Intl.DateTimeFormat().resolvedOptions().timeZone || 'Europe/Paris'; } catch (_) { /* Keep the documented default. */ }
    const render = () => {
      review.textContent = topic && slot ? `${topic} · ${slot} · ${zone.textContent}` : 'Choose a purpose and sample time.';
    };
    topicButtons.forEach((button) => button.addEventListener('click', () => { topic = button.dataset.meetingTopic; topicButtons.forEach((item) => item.setAttribute('aria-pressed', String(item === button))); render(); }));
    slotButtons.forEach((button) => button.addEventListener('click', () => { slot = button.dataset.meetingSlot; slotButtons.forEach((item) => item.setAttribute('aria-pressed', String(item === button))); render(); }));
    confirm?.addEventListener('click', () => {
      status.textContent = topic && slot ? 'Demo complete. No appointment was created and no calendar was contacted.' : 'Select both a purpose and a sample time to complete the demo.';
    });
  });

  document.querySelectorAll('.file-test').forEach((button) => button.addEventListener('click', async () => {
    const status = document.querySelector('.file-test-status');
    if (status) status.textContent = 'Checking…';
    try {
      const response = await fetch(button.dataset.testUrl, { method: 'HEAD' });
      if (!response.ok) throw new Error(String(response.status));
      if (status) status.textContent = `Available: ${button.dataset.testUrl}`;
    } catch (_) { if (status) status.textContent = `Controlled error: ${button.dataset.testUrl} is not available.`; }
  }));

  document.querySelectorAll('[data-typography-select]').forEach((select) => {
    const samples = [...document.querySelectorAll(`#type-samples-${select.dataset.typographySelect} [data-type-sample]`)];
    const updateSample = () => samples.forEach((sample) => { sample.hidden = sample.dataset.typeSample !== select.value; });
    select.addEventListener('change', updateSample);
    updateSample();
  });

  document.querySelectorAll('.immersive-open').forEach((openButton) => {
    const dialog = document.getElementById(openButton.dataset.immersiveTarget || '');
    const sourceStanzas = [...document.querySelectorAll('.poem-text .poem-stanza')];
    if (!dialog || !sourceStanzas.length || typeof dialog.showModal !== 'function') return;
    const page = dialog.querySelector('.immersive-page');
    const progress = dialog.querySelector('.immersive-progress');
    const previous = dialog.querySelector('.immersive-prev');
    const next = dialog.querySelector('.immersive-next');
    const close = dialog.querySelector('.immersive-close');
    let index = 0;
    const renderPage = () => {
      page.replaceChildren(sourceStanzas[index].cloneNode(true));
      progress.textContent = `Stanza ${index + 1} of ${sourceStanzas.length}`;
      previous.disabled = index === 0;
      next.textContent = index === sourceStanzas.length - 1 ? 'Finish' : 'Next';
      page.focus();
    };
    openButton.addEventListener('click', () => { index = 0; renderPage(); dialog.showModal(); });
    close.addEventListener('click', () => dialog.close());
    previous.addEventListener('click', () => { if (index > 0) { index -= 1; renderPage(); } });
    next.addEventListener('click', () => { if (index < sourceStanzas.length - 1) { index += 1; renderPage(); } else dialog.close(); });
    dialog.addEventListener('click', (event) => { if (event.target === dialog) dialog.close(); });
    dialog.addEventListener('close', () => openButton.focus());
    dialog.addEventListener('keydown', (event) => {
      if (event.key === 'ArrowLeft' && index > 0) { index -= 1; renderPage(); }
      if (event.key === 'ArrowRight') { if (index < sourceStanzas.length - 1) { index += 1; renderPage(); } else dialog.close(); }
    });
  });
})();
