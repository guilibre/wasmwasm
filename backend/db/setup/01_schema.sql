ALTER SESSION
SET
    CURRENT_SCHEMA = backwasm;

CREATE TABLE
    users (
        id NUMBER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
        username VARCHAR2(50) NOT NULL UNIQUE,
        password_hash VARCHAR2(255) NOT NULL,
        created_at TIMESTAMP DEFAULT SYSTIMESTAMP NOT NULL
    );

CREATE TABLE
    patches (
        id NUMBER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
        user_id NUMBER NOT NULL,
        title VARCHAR2(120) NOT NULL,
        is_public NUMBER(1) DEFAULT 0 NOT NULL,
        current_version_id NUMBER,
        created_at TIMESTAMP DEFAULT SYSTIMESTAMP NOT NULL,
        updated_at TIMESTAMP DEFAULT SYSTIMESTAMP NOT NULL,
        CONSTRAINT fk_patches_user FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE,
        CONSTRAINT chk_patches_is_public CHECK (is_public IN (0, 1))
    );

CREATE TABLE
    patch_versions (
        id NUMBER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
        patch_id NUMBER NOT NULL,
        version_number NUMBER NOT NULL,
        data JSON NOT NULL,
        created_at TIMESTAMP DEFAULT SYSTIMESTAMP NOT NULL,
        CONSTRAINT fk_patch_versions_patch FOREIGN KEY (patch_id) REFERENCES patches (id) ON DELETE CASCADE,
        CONSTRAINT uq_patch_versions_patch_version UNIQUE (patch_id, version_number)
    );

ALTER TABLE patches
ADD CONSTRAINT fk_patches_current_version FOREIGN KEY (current_version_id) REFERENCES patch_versions (id);
