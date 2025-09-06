package libs.core.seqvar;

import java.io.File;
import java.sql.*;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Java implementation of the SeqVar store - a simple key-value configuration system
 * for SeqWeb development environment.
 * 
 * This class provides persistent storage of configuration values with source tracking
 * and timestamps, similar to the Python seqvar implementation.
 */
public class SeqVar {
    
    /**
     * Exception thrown by SeqVar operations
     */
    public static class SeqVarException extends RuntimeException {
        public SeqVarException(String message) {
            super(message);
        }
        
        public SeqVarException(String message, Throwable cause) {
            super(message, cause);
        }
    }
    
    private static final String DB_NAME = "seqvar.sqlite";
    private static final String SCHEMA_SQL = """
        PRAGMA journal_mode=WAL;
        PRAGMA synchronous=NORMAL;
        CREATE TABLE IF NOT EXISTS seqvars(
          key  TEXT NOT NULL,
          val  TEXT NOT NULL DEFAULT '',
          src  TEXT,
          ts   INTEGER NOT NULL,
          PRIMARY KEY(key)
        );
        """;
    
    /**
     * Get a seqvar value by key
     * 
     * @param key the configuration key
     * @return the value, or null if not found
     * @throws SeqVarException if there's a database error
     */
    public static String get(String key) throws SeqVarException {
        try (Connection conn = conn();
             PreparedStatement stmt = conn.prepareStatement(
                 "SELECT val FROM seqvars WHERE key = ?")) {
            
            stmt.setString(1, key);
            ResultSet rs = stmt.executeQuery();
            
            if (rs.next()) {
                return rs.getString("val");
            }
            return null;
            
        } catch (SQLException e) {
            throw new SeqVarException("Error getting seqvar '" + key + "': " + e.getMessage(), e);
        }
    }
    
    /**
     * Set a seqvar value
     * 
     * @param key the configuration key
     * @param value the value to set
     * @param src the source of this value (can be null to auto-detect)
     * @throws SeqVarException if there's a database error
     */
    public static void set(String key, String value, String src) throws SeqVarException {
        if (key == null || key.trim().isEmpty()) {
            throw new SeqVarException("Key cannot be null or empty");
        }
        
        if (value == null) {
            value = "";
        }
        
        // Auto-detect source if not provided
        if (src == null) {
            src = inferSource();
        }
        
        long timestamp = Instant.now().getEpochSecond();
        
        try (Connection conn = conn();
             PreparedStatement stmt = conn.prepareStatement(
                 "INSERT OR REPLACE INTO seqvars (key, val, src, ts) VALUES (?, ?, ?, ?)")) {
            
            stmt.setString(1, key);
            stmt.setString(2, value);
            stmt.setString(3, src);
            stmt.setLong(4, timestamp);
            
            stmt.executeUpdate();
            
        } catch (SQLException e) {
            throw new SeqVarException("Error setting seqvar '" + key + "': " + e.getMessage(), e);
        }
    }
    
    /**
     * Set a seqvar value with auto-detected source
     * 
     * @param key the configuration key
     * @param value the value to set
     * @throws SeqVarException if there's a database error
     */
    public static void set(String key, String value) throws SeqVarException {
        set(key, value, null);
    }
    
    /**
     * Get all seqvar entries as a list of arrays [key, value, source, timestamp]
     * 
     * @return list of entries
     * @throws SeqVarException if there's a database error
     */
    public static List<Object[]> dump() throws SeqVarException {
        List<Object[]> results = new ArrayList<>();
        
        try (Connection conn = conn();
             PreparedStatement stmt = conn.prepareStatement(
                 "SELECT key, val, src, ts FROM seqvars ORDER BY key")) {
            
            ResultSet rs = stmt.executeQuery();
            
            while (rs.next()) {
                Object[] row = {
                    rs.getString("key"),
                    rs.getString("val"),
                    rs.getString("src"),
                    rs.getLong("ts")
                };
                results.add(row);
            }
            
        } catch (SQLException e) {
            throw new SeqVarException("Error dumping seqvars: " + e.getMessage(), e);
        }
        
        return results;
    }
    
    /**
     * Get seqvar entries as a Map, optionally filtered by pattern
     * 
     * @param pattern SQL LIKE pattern (e.g., "repos.*"), or null for all
     * @return map of key-value pairs
     * @throws SeqVarException if there's a database error
     */
    public static Map<String, String> getDict(String pattern) throws SeqVarException {
        Map<String, String> results = new HashMap<>();
        
        String sql = "SELECT key, val FROM seqvars";
        if (pattern != null && !pattern.trim().isEmpty()) {
            sql += " WHERE key LIKE ?";
        }
        sql += " ORDER BY key";
        
        try (Connection conn = conn();
             PreparedStatement stmt = conn.prepareStatement(sql)) {
            
            if (pattern != null && !pattern.trim().isEmpty()) {
                stmt.setString(1, pattern);
            }
            
            ResultSet rs = stmt.executeQuery();
            
            while (rs.next()) {
                results.put(rs.getString("key"), rs.getString("val"));
            }
            
        } catch (SQLException e) {
            throw new SeqVarException("Error getting seqvar dict: " + e.getMessage(), e);
        }
        
        return results;
    }
    
    /**
     * Get all seqvar entries as a Map
     * 
     * @return map of all key-value pairs
     * @throws SeqVarException if there's a database error
     */
    public static Map<String, String> getDict() throws SeqVarException {
        return getDict(null);
    }
    
    /**
     * Get the path to the seqvar store database
     * 
     * @return the database file path
     */
    public static File seqvarStorePath() {
        String home = System.getenv("SEQWEBDEV_HOME");
        if (home == null) {
            throw new SeqVarException("SEQWEBDEV_HOME environment variable is not set");
        }
        
        File stateDir = new File(home, ".state");
        return new File(stateDir, DB_NAME);
    }
    
    /**
     * Get a database connection, creating the database and schema if needed
     * 
     * @return database connection
     * @throws SQLException if connection fails
     */
    private static Connection conn() throws SQLException {
        File dbFile = seqvarStorePath();
        File dbDir = dbFile.getParentFile();
        
        // Create directory if it doesn't exist
        if (!dbDir.exists() && !dbDir.mkdirs()) {
            throw new SeqVarException("Could not create directory: " + dbDir);
        }
        
        // Create database and schema if it doesn't exist
        if (!dbFile.exists()) {
            createDatabase(dbFile);
        }
        
        return DriverManager.getConnection("jdbc:sqlite:" + dbFile.getAbsolutePath());
    }
    
    /**
     * Create the database and schema
     * 
     * @param dbFile the database file to create
     * @throws SeqVarException if creation fails
     */
    private static void createDatabase(File dbFile) throws SeqVarException {
        try (Connection conn = DriverManager.getConnection("jdbc:sqlite:" + dbFile.getAbsolutePath())) {
            
            // Apply schema
            for (String stmt : SCHEMA_SQL.split(";")) {
                String trimmed = stmt.trim();
                if (!trimmed.isEmpty()) {
                    try (Statement sqlStmt = conn.createStatement()) {
                        sqlStmt.execute(trimmed);
                    }
                }
            }
            
        } catch (SQLException e) {
            throw new SeqVarException("Failed to create database: " + e.getMessage(), e);
        }
    }
    
    /**
     * Infer the source from the current stack trace
     * 
     * @return the inferred source name
     */
    private static String inferSource() {
        StackTraceElement[] stack = Thread.currentThread().getStackTrace();
        
        // Skip the first few frames (getStackTrace, inferSource, set)
        for (int i = 3; i < stack.length; i++) {
            StackTraceElement frame = stack[i];
            String className = frame.getClassName();
            
            // Skip internal Java classes and our own SeqVar class
            if (!className.startsWith("java.") && 
                !className.startsWith("lib.seqvar.SeqVar") &&
                !className.startsWith("sun.")) {
                
                // Extract just the class name without package
                String[] parts = className.split("\\.");
                return parts[parts.length - 1];
            }
        }
        
        return "unknown";
    }
}
