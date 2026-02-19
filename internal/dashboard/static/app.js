/**
 * Anvil Dashboard - Real-time Migration Monitoring
 * Pure vanilla JavaScript application for monitoring legacy code migrations
 * Security: Uses safe DOM methods, no innerHTML with user data
 */

class AnvilDashboard {
    constructor() {
        this.eventSource = null;
        this.reconnectAttempts = 0;
        this.maxReconnectAttempts = 10;
        this.reconnectDelay = 1000; // Start with 1 second
        this.migrations = new Map();
        this.expandedRows = new Set();
        this.autoScroll = true;

        this.init();
    }

    init() {
        this.setupEventListeners();
        this.connectSSE();
        this.fetchInitialData();
        this.startPeriodicRefresh();
        this.addLog('Dashboard initialized', 'info');
    }

    setupEventListeners() {
        // Clear logs button
        document.getElementById('btnClearLogs').addEventListener('click', () => {
            this.clearLogs();
        });

        // Auto-scroll detection for logs
        const logsContainer = document.getElementById('logsContainer');
        logsContainer.addEventListener('scroll', () => {
            const isScrolledToBottom = logsContainer.scrollHeight - logsContainer.scrollTop <= logsContainer.clientHeight + 50;
            this.autoScroll = isScrolledToBottom;
        });
    }

    connectSSE() {
        this.updateConnectionStatus('reconnecting');
        this.addLog('Connecting to event stream...', 'info');

        try {
            this.eventSource = new EventSource('/api/events');

            this.eventSource.onopen = () => {
                this.reconnectAttempts = 0;
                this.reconnectDelay = 1000;
                this.updateConnectionStatus('connected');
                this.addLog('Connected to event stream', 'success');
            };

            this.eventSource.onerror = (error) => {
                console.error('SSE error:', error);
                this.updateConnectionStatus('disconnected');
                this.addLog('Connection lost, attempting to reconnect...', 'error');

                if (this.eventSource) {
                    this.eventSource.close();
                }

                this.scheduleReconnect();
            };

            this.eventSource.addEventListener('migration_start', (e) => {
                this.handleMigrationStart(JSON.parse(e.data));
            });

            this.eventSource.addEventListener('migration_update', (e) => {
                this.handleMigrationUpdate(JSON.parse(e.data));
            });

            this.eventSource.addEventListener('migration_complete', (e) => {
                this.handleMigrationComplete(JSON.parse(e.data));
            });

            this.eventSource.addEventListener('stats_update', (e) => {
                this.handleStatsUpdate(JSON.parse(e.data));
            });

        } catch (error) {
            console.error('Failed to connect SSE:', error);
            this.updateConnectionStatus('disconnected');
            this.scheduleReconnect();
        }
    }

    scheduleReconnect() {
        if (this.reconnectAttempts >= this.maxReconnectAttempts) {
            this.addLog(`Max reconnection attempts (${this.maxReconnectAttempts}) reached. Please refresh the page.`, 'error');
            return;
        }

        this.reconnectAttempts++;
        const delay = Math.min(this.reconnectDelay * Math.pow(2, this.reconnectAttempts - 1), 30000);

        this.addLog(`Reconnecting in ${(delay / 1000).toFixed(1)}s (attempt ${this.reconnectAttempts}/${this.maxReconnectAttempts})...`, 'warning');

        setTimeout(() => {
            this.connectSSE();
        }, delay);
    }

    updateConnectionStatus(status) {
        const indicator = document.getElementById('statusIndicator');
        const text = document.getElementById('statusText');

        indicator.className = 'status-indicator ' + status;

        switch (status) {
            case 'connected':
                text.textContent = 'Connected';
                break;
            case 'disconnected':
                text.textContent = 'Disconnected';
                break;
            case 'reconnecting':
                text.textContent = 'Connecting...';
                break;
        }
    }

    async fetchInitialData() {
        try {
            // Fetch stats
            const statsResponse = await fetch('/api/stats');
            if (statsResponse.ok) {
                const stats = await statsResponse.json();
                this.updateStats(stats);
            }

            // Fetch migrations
            const migrationsResponse = await fetch('/api/migrations');
            if (migrationsResponse.ok) {
                const migrations = await migrationsResponse.json();
                migrations.forEach(migration => {
                    this.migrations.set(migration.id, migration);
                });
                this.renderMigrationsTable();
            }
        } catch (error) {
            console.error('Failed to fetch initial data:', error);
            this.addLog('Failed to load initial data', 'error');
        }
    }

    startPeriodicRefresh() {
        setInterval(async () => {
            try {
                const response = await fetch('/api/stats');
                if (response.ok) {
                    const stats = await response.json();
                    this.updateStats(stats);
                }
            } catch (error) {
                console.error('Periodic refresh failed:', error);
            }
        }, 30000); // Every 30 seconds
    }

    handleMigrationStart(data) {
        this.migrations.set(data.id, data);
        this.renderMigrationsTable();
        this.addLog(`Migration started: ${data.name} (${data.source_lang} → ${data.target_lang})`, 'info');
        this.showToast('Migration Started', `${data.name}`, 'info');
    }

    handleMigrationUpdate(data) {
        const migration = this.migrations.get(data.id);
        if (migration) {
            Object.assign(migration, data);
            this.renderMigrationsTable();

            if (data.current_stage) {
                this.addLog(`[${data.name}] Stage: ${data.current_stage} - ${data.status}`, 'info');
            }
        }
    }

    handleMigrationComplete(data) {
        const migration = this.migrations.get(data.id);
        if (migration) {
            Object.assign(migration, data);
            this.renderMigrationsTable();

            const success = data.status === 'completed';
            this.addLog(
                `Migration ${success ? 'completed' : 'failed'}: ${data.name} (${this.formatDuration(data.duration)})`,
                success ? 'success' : 'error'
            );
            this.showToast(
                success ? 'Migration Completed' : 'Migration Failed',
                `${data.name} - ${this.formatDuration(data.duration)}`,
                success ? 'success' : 'error'
            );
        }
    }

    handleStatsUpdate(stats) {
        this.updateStats(stats);
    }

    updateStats(stats) {
        document.getElementById('statTotal').textContent = stats.total || 0;
        document.getElementById('statActive').textContent = stats.active || 0;
        document.getElementById('statCompleted').textContent = stats.completed || 0;
        document.getElementById('statFailed').textContent = stats.failed || 0;

        const successRate = stats.total > 0 ? ((stats.completed / stats.total) * 100).toFixed(1) : '0.0';
        document.getElementById('statSuccessRate').textContent = successRate + '%';

        document.getElementById('statLLMCalls').textContent = (stats.llm_calls || 0).toLocaleString();
    }

    renderMigrationsTable() {
        const tbody = document.getElementById('migrationsTableBody');

        if (this.migrations.size === 0) {
            tbody.replaceChildren(this.createEmptyStateRow());
            return;
        }

        // Sort migrations by start time (most recent first)
        const sortedMigrations = Array.from(this.migrations.values())
            .sort((a, b) => new Date(b.started_at) - new Date(a.started_at));

        // Clear and rebuild table
        tbody.replaceChildren();
        sortedMigrations.forEach(migration => {
            tbody.appendChild(this.createMigrationRow(migration));

            if (this.expandedRows.has(migration.id)) {
                tbody.appendChild(this.createPipelineRow(migration));
            }
        });
    }

    createEmptyStateRow() {
        const row = document.createElement('tr');
        row.className = 'empty-state';

        const td = document.createElement('td');
        td.colSpan = 7;

        const content = document.createElement('div');
        content.className = 'empty-state-content';

        const p1 = document.createElement('p');
        p1.textContent = 'No migrations yet';

        const p2 = document.createElement('p');
        p2.className = 'empty-state-hint';
        p2.textContent = 'Migrations will appear here when they start';

        content.appendChild(p1);
        content.appendChild(p2);
        td.appendChild(content);
        row.appendChild(td);

        return row;
    }

    createMigrationRow(migration) {
        const row = document.createElement('tr');
        row.dataset.id = migration.id;

        if (this.expandedRows.has(migration.id)) {
            row.classList.add('expanded');
        }

        const statusClass = this.getStatusClass(migration.status);
        const progress = this.calculateProgress(migration);
        const duration = this.getDuration(migration);

        // ID column
        const tdId = document.createElement('td');
        tdId.className = 'text-monospace';
        tdId.textContent = this.truncateId(migration.id);
        row.appendChild(tdId);

        // Name column
        const tdName = document.createElement('td');
        tdName.textContent = migration.name;
        row.appendChild(tdName);

        // Source -> Target column
        const tdLang = document.createElement('td');
        tdLang.className = 'text-monospace';
        tdLang.textContent = `${migration.source_lang} → ${migration.target_lang}`;
        row.appendChild(tdLang);

        // Status column
        const tdStatus = document.createElement('td');
        const statusBadge = document.createElement('span');
        statusBadge.className = `status-badge ${statusClass}`;
        statusBadge.textContent = migration.status;
        tdStatus.appendChild(statusBadge);
        row.appendChild(tdStatus);

        // Duration column
        const tdDuration = document.createElement('td');
        tdDuration.className = 'text-monospace';
        tdDuration.textContent = this.formatDuration(duration);
        row.appendChild(tdDuration);

        // Progress column
        const tdProgress = document.createElement('td');
        tdProgress.appendChild(this.createProgressBar(progress));
        row.appendChild(tdProgress);

        // Actions column
        const tdActions = document.createElement('td');
        const actionBtn = document.createElement('button');
        actionBtn.className = 'action-btn';
        actionBtn.onclick = () => this.toggleExpand(migration.id);

        const expandIcon = document.createElement('span');
        expandIcon.className = `expand-icon ${this.expandedRows.has(migration.id) ? 'expanded' : ''}`;
        expandIcon.textContent = '▼';

        actionBtn.appendChild(expandIcon);
        tdActions.appendChild(actionBtn);
        row.appendChild(tdActions);

        return row;
    }

    createProgressBar(progress) {
        const container = document.createElement('div');
        container.className = 'progress-bar';

        const track = document.createElement('div');
        track.className = 'progress-track';

        const fill = document.createElement('div');
        fill.className = 'progress-fill';
        fill.style.width = `${progress.percentage}%`;

        track.appendChild(fill);

        const text = document.createElement('span');
        text.className = 'progress-text';
        text.textContent = `${progress.completed}/${progress.total}`;

        container.appendChild(track);
        container.appendChild(text);

        return container;
    }

    createPipelineRow(migration) {
        const row = document.createElement('tr');
        row.classList.add('pipeline-details');

        const td = document.createElement('td');
        td.colSpan = 7;

        const pipelineContainer = document.createElement('div');
        pipelineContainer.className = 'pipeline-container';

        const stages = ['cartographer', 'specular', 'architect', 'judge', 'testgen'];
        const stageLabels = {
            'cartographer': 'Cartographer',
            'specular': 'Specular',
            'architect': 'Architect',
            'judge': 'Judge',
            'testgen': 'TestGen'
        };

        stages.forEach((stageName, index) => {
            const stageData = migration.stages ? migration.stages[stageName] : null;
            const stageStatus = this.getStageStatus(migration, stageName);

            pipelineContainer.appendChild(this.createPipelineStage(stageName, stageLabels[stageName], stageStatus, stageData));

            if (index < stages.length - 1) {
                const connectorClass = this.getConnectorClass(migration, stageName, stages[index + 1]);
                pipelineContainer.appendChild(this.createPipelineConnector(connectorClass));
            }
        });

        td.appendChild(pipelineContainer);
        row.appendChild(td);

        return row;
    }

    createPipelineStage(stageName, label, status, stageData) {
        const stageDiv = document.createElement('div');
        stageDiv.className = 'pipeline-stage';

        const nameDiv = document.createElement('div');
        nameDiv.className = 'stage-name';
        nameDiv.textContent = label;

        const nodeDiv = document.createElement('div');
        const statusClass = this.getStatusClass(status);
        nodeDiv.className = `stage-node ${statusClass.replace('status-', 'stage-')}`;
        nodeDiv.textContent = this.getStageIcon(status);

        stageDiv.appendChild(nameDiv);
        stageDiv.appendChild(nodeDiv);

        if (stageData && stageData.duration) {
            const durationDiv = document.createElement('div');
            durationDiv.className = 'stage-duration';
            durationDiv.textContent = this.formatDuration(stageData.duration);
            stageDiv.appendChild(durationDiv);
        }

        return stageDiv;
    }

    createPipelineConnector(className) {
        const connector = document.createElement('div');
        connector.className = `pipeline-connector ${className}`;
        return connector;
    }

    getStageStatus(migration, stageName) {
        if (!migration.stages || !migration.stages[stageName]) {
            return migration.current_stage === stageName ? 'running' : 'pending';
        }
        return migration.stages[stageName].status || 'pending';
    }

    getStageIcon(status) {
        switch (status) {
            case 'completed':
                return '✓';
            case 'running':
                return '●';
            case 'failed':
                return '✗';
            default:
                return '○';
        }
    }

    getConnectorClass(migration, currentStage, nextStage) {
        const currentStatus = this.getStageStatus(migration, currentStage);
        const nextStatus = this.getStageStatus(migration, nextStage);

        if (currentStatus === 'completed' && nextStatus === 'running') {
            return 'connector-running';
        } else if (currentStatus === 'completed' && nextStatus === 'completed') {
            return 'connector-completed';
        }
        return '';
    }

    toggleExpand(migrationId) {
        if (this.expandedRows.has(migrationId)) {
            this.expandedRows.delete(migrationId);
        } else {
            this.expandedRows.add(migrationId);
        }
        this.renderMigrationsTable();
    }

    calculateProgress(migration) {
        const stages = ['cartographer', 'specular', 'architect', 'judge', 'testgen'];
        let completed = 0;

        if (migration.stages) {
            stages.forEach(stage => {
                if (migration.stages[stage] && migration.stages[stage].status === 'completed') {
                    completed++;
                }
            });
        }

        return {
            completed,
            total: stages.length,
            percentage: (completed / stages.length) * 100
        };
    }

    getDuration(migration) {
        if (migration.duration) {
            return migration.duration;
        }

        if (migration.started_at) {
            const start = new Date(migration.started_at);
            const end = migration.completed_at ? new Date(migration.completed_at) : new Date();
            return Math.floor((end - start) / 1000); // Duration in seconds
        }

        return 0;
    }

    formatDuration(seconds) {
        if (!seconds || seconds < 0) return '-';

        const hours = Math.floor(seconds / 3600);
        const minutes = Math.floor((seconds % 3600) / 60);
        const secs = Math.floor(seconds % 60);

        if (hours > 0) {
            return `${hours}h ${minutes}m ${secs}s`;
        } else if (minutes > 0) {
            return `${minutes}m ${secs}s`;
        } else {
            return `${secs}s`;
        }
    }

    formatTimestamp(date) {
        if (!date) return '';

        const d = new Date(date);
        const now = new Date();
        const diffMs = now - d;
        const diffSecs = Math.floor(diffMs / 1000);
        const diffMins = Math.floor(diffSecs / 60);
        const diffHours = Math.floor(diffMins / 60);
        const diffDays = Math.floor(diffHours / 24);

        if (diffSecs < 60) {
            return 'just now';
        } else if (diffMins < 60) {
            return `${diffMins} minute${diffMins !== 1 ? 's' : ''} ago`;
        } else if (diffHours < 24) {
            return `${diffHours} hour${diffHours !== 1 ? 's' : ''} ago`;
        } else if (diffDays < 7) {
            return `${diffDays} day${diffDays !== 1 ? 's' : ''} ago`;
        } else {
            return d.toLocaleDateString();
        }
    }

    getStatusClass(status) {
        const statusMap = {
            'running': 'status-running',
            'completed': 'status-completed',
            'failed': 'status-failed',
            'pending': 'status-pending'
        };
        return statusMap[status] || 'status-pending';
    }

    truncateId(id) {
        if (!id) return '-';
        return id.length > 8 ? id.substring(0, 8) + '...' : id;
    }

    addLog(message, type = 'info') {
        const logsContainer = document.getElementById('logsContainer');
        const logEntry = document.createElement('div');
        logEntry.className = `log-entry log-${type}`;

        const now = new Date();
        const timestamp = now.toLocaleTimeString('en-US', { hour12: false });

        const timestampSpan = document.createElement('span');
        timestampSpan.className = 'log-timestamp';
        timestampSpan.textContent = `[${timestamp}]`;

        const messageSpan = document.createElement('span');
        messageSpan.className = 'log-message';
        messageSpan.textContent = message;

        logEntry.appendChild(timestampSpan);
        logEntry.appendChild(messageSpan);
        logsContainer.appendChild(logEntry);

        // Keep only last 500 log entries
        while (logsContainer.children.length > 500) {
            logsContainer.removeChild(logsContainer.firstChild);
        }

        // Auto-scroll to bottom if enabled
        if (this.autoScroll) {
            logsContainer.scrollTop = logsContainer.scrollHeight;
        }
    }

    clearLogs() {
        const logsContainer = document.getElementById('logsContainer');
        logsContainer.replaceChildren();
        this.addLog('Logs cleared', 'info');
    }

    showToast(title, message, type = 'info') {
        const container = document.getElementById('toastContainer');
        const toast = document.createElement('div');
        toast.className = `toast toast-${type}`;

        const titleDiv = document.createElement('div');
        titleDiv.className = 'toast-title';
        titleDiv.textContent = title;

        const messageDiv = document.createElement('div');
        messageDiv.className = 'toast-message';
        messageDiv.textContent = message;

        toast.appendChild(titleDiv);
        toast.appendChild(messageDiv);
        container.appendChild(toast);

        // Auto-remove after 5 seconds
        setTimeout(() => {
            toast.classList.add('removing');
            setTimeout(() => {
                if (container.contains(toast)) {
                    container.removeChild(toast);
                }
            }, 300);
        }, 5000);
    }

    destroy() {
        if (this.eventSource) {
            this.eventSource.close();
        }
    }
}

// Initialize dashboard when DOM is ready
let dashboard;

if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', () => {
        dashboard = new AnvilDashboard();
    });
} else {
    dashboard = new AnvilDashboard();
}

// Cleanup on page unload
window.addEventListener('beforeunload', () => {
    if (dashboard) {
        dashboard.destroy();
    }
});
