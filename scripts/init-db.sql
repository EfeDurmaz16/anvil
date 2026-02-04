-- Anvil Test Database Initialization
-- This script runs on first PostgreSQL container startup

-- Create schemas for test fixtures
CREATE SCHEMA IF NOT EXISTS source_system;
CREATE SCHEMA IF NOT EXISTS target_system;

-- Example tables for DB diff testing (banking domain)
-- These mirror typical COBOL data structures

-- Accounts table (source system - simulating COBOL data)
CREATE TABLE IF NOT EXISTS source_system.accounts (
    account_id VARCHAR(20) PRIMARY KEY,
    customer_id VARCHAR(20) NOT NULL,
    account_type VARCHAR(10) NOT NULL,
    balance DECIMAL(15, 2) NOT NULL DEFAULT 0.00,
    currency VARCHAR(3) NOT NULL DEFAULT 'USD',
    status VARCHAR(10) NOT NULL DEFAULT 'ACTIVE',
    opened_date DATE NOT NULL,
    last_updated TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Transactions table (source system)
CREATE TABLE IF NOT EXISTS source_system.transactions (
    transaction_id VARCHAR(30) PRIMARY KEY,
    account_id VARCHAR(20) NOT NULL REFERENCES source_system.accounts(account_id),
    transaction_type VARCHAR(10) NOT NULL,
    amount DECIMAL(15, 2) NOT NULL,
    transaction_date TIMESTAMP NOT NULL,
    description VARCHAR(100),
    status VARCHAR(10) NOT NULL DEFAULT 'COMPLETED'
);

-- Accounts table (target system - modernized)
CREATE TABLE IF NOT EXISTS target_system.accounts (
    account_id VARCHAR(20) PRIMARY KEY,
    customer_id VARCHAR(20) NOT NULL,
    account_type VARCHAR(10) NOT NULL,
    balance DECIMAL(15, 2) NOT NULL DEFAULT 0.00,
    currency VARCHAR(3) NOT NULL DEFAULT 'USD',
    status VARCHAR(10) NOT NULL DEFAULT 'ACTIVE',
    opened_date DATE NOT NULL,
    last_updated TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Transactions table (target system)
CREATE TABLE IF NOT EXISTS target_system.transactions (
    transaction_id VARCHAR(30) PRIMARY KEY,
    account_id VARCHAR(20) NOT NULL REFERENCES target_system.accounts(account_id),
    transaction_type VARCHAR(10) NOT NULL,
    amount DECIMAL(15, 2) NOT NULL,
    transaction_date TIMESTAMP NOT NULL,
    description VARCHAR(100),
    status VARCHAR(10) NOT NULL DEFAULT 'COMPLETED'
);

-- Insert sample test data
INSERT INTO source_system.accounts (account_id, customer_id, account_type, balance, currency, status, opened_date)
VALUES
    ('ACC001', 'CUST001', 'CHECKING', 1500.00, 'USD', 'ACTIVE', '2020-01-15'),
    ('ACC002', 'CUST001', 'SAVINGS', 25000.00, 'USD', 'ACTIVE', '2020-01-15'),
    ('ACC003', 'CUST002', 'CHECKING', 3200.50, 'USD', 'ACTIVE', '2021-06-20')
ON CONFLICT DO NOTHING;

INSERT INTO source_system.transactions (transaction_id, account_id, transaction_type, amount, transaction_date, description, status)
VALUES
    ('TXN001', 'ACC001', 'DEPOSIT', 500.00, '2024-01-10 10:30:00', 'Direct Deposit', 'COMPLETED'),
    ('TXN002', 'ACC001', 'WITHDRAW', -100.00, '2024-01-11 14:20:00', 'ATM Withdrawal', 'COMPLETED'),
    ('TXN003', 'ACC002', 'DEPOSIT', 1000.00, '2024-01-12 09:00:00', 'Transfer In', 'COMPLETED')
ON CONFLICT DO NOTHING;

-- Copy data to target system for initial state
INSERT INTO target_system.accounts SELECT * FROM source_system.accounts ON CONFLICT DO NOTHING;
INSERT INTO target_system.transactions SELECT * FROM source_system.transactions ON CONFLICT DO NOTHING;

-- Create indexes for performance
CREATE INDEX IF NOT EXISTS idx_src_accounts_customer ON source_system.accounts(customer_id);
CREATE INDEX IF NOT EXISTS idx_src_transactions_account ON source_system.transactions(account_id);
CREATE INDEX IF NOT EXISTS idx_src_transactions_date ON source_system.transactions(transaction_date);

CREATE INDEX IF NOT EXISTS idx_tgt_accounts_customer ON target_system.accounts(customer_id);
CREATE INDEX IF NOT EXISTS idx_tgt_transactions_account ON target_system.transactions(account_id);
CREATE INDEX IF NOT EXISTS idx_tgt_transactions_date ON target_system.transactions(transaction_date);

-- Grant permissions
GRANT ALL PRIVILEGES ON SCHEMA source_system TO anvil;
GRANT ALL PRIVILEGES ON SCHEMA target_system TO anvil;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA source_system TO anvil;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA target_system TO anvil;
