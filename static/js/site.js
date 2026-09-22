(() => {
  document.querySelectorAll('[data-filter-root]').forEach((root) => {
    const buttons = [...root.querySelectorAll('[data-filter]')];
    const items = [...root.querySelectorAll('[data-filter-item]')];
    buttons.forEach((button) => {
      button.addEventListener('click', () => {
        const filter = button.dataset.filter;
        buttons.forEach((candidate) => {
          const active = candidate === button;
          candidate.classList.toggle('active', active);
          candidate.setAttribute('aria-pressed', String(active));
        });
        items.forEach((item) => {
          item.hidden = filter !== 'all' && item.dataset.filterItem !== filter;
        });
      });
    });
  });

  document.querySelectorAll('.copy-citation').forEach((button) => {
    button.addEventListener('click', async () => {
      const status = button.closest('header')?.querySelector('.copy-status');
      try {
        await navigator.clipboard.writeText(button.dataset.citation || '');
        if (status) status.textContent = 'Preview citation copied.';
      } catch {
        if (status) status.textContent = 'Copying was unavailable. Select the citation text manually.';
      }
    });
  });

  document.querySelectorAll('.file-test').forEach((button) => {
    button.addEventListener('click', async () => {
      const status = document.querySelector('.file-test-status');
      if (status) status.textContent = 'Checking…';
      try {
        const response = await fetch(button.dataset.testUrl, { method: 'HEAD' });
        if (!response.ok) throw new Error(String(response.status));
        if (status) status.textContent = `Available: ${button.dataset.testUrl}`;
      } catch {
        if (status) status.textContent = `Controlled error: ${button.dataset.testUrl} is not available.`;
      }
    });
  });

  document.querySelectorAll('.mobile-nav a').forEach((link) => {
    link.addEventListener('click', () => link.closest('details')?.removeAttribute('open'));
  });
})();
