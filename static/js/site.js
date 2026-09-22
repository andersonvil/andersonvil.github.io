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

  document.querySelectorAll('.file-test').forEach((button) => button.addEventListener('click', async () => {
    const status = document.querySelector('.file-test-status');
    if (status) status.textContent = 'Checking…';
    try {
      const response = await fetch(button.dataset.testUrl, { method: 'HEAD' });
      if (!response.ok) throw new Error(String(response.status));
      if (status) status.textContent = `Available: ${button.dataset.testUrl}`;
    } catch (_) { if (status) status.textContent = `Controlled error: ${button.dataset.testUrl} is not available.`; }
  }));
})();
