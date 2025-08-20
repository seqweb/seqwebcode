// Created by Waldo 2025-08-20

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.tomlj.Toml;
import org.tomlj.TomlParseResult;
import org.tomlj.TomlTable;

/**
 * TOML sidecar for SeqVar Facility - loads TOML files and writes to seqvar database.
 * Implements the common contract: flattens tables to dotted keys, stringifies values.
 */
public final class SeqvarToml {
    
    /**
     * Recursively flatten TOML data structure into dotted key-value pairs.
     * All values are converted to strings to maintain seqvar DB uniformity.
     */
    private static Map<String, String> flattenToml(Object data, String prefix) {
        Map<String, String> result = new HashMap<>();
        
        if (data instanceof Map) {
            @SuppressWarnings("unchecked")
            Map<String, Object> map = (Map<String, Object>) data;
            for (Map.Entry<String, Object> entry : map.entrySet()) {
                String newPrefix = prefix.isEmpty() ? entry.getKey() : prefix + "." + entry.getKey();
                result.putAll(flattenToml(entry.getValue(), newPrefix));
            }
        } else if (data instanceof List) {
            // Convert lists to JSON-like string representation
            result.put(prefix, data.toString());
        } else {
            // Convert any other type to string
            result.put(prefix, String.valueOf(data));
        }
        
        return result;
    }
    
    /**
     * Load TOML file(s) and return flattened key-value map.
     * 
     * @param paths Single path or list of paths to TOML files
     * @return Map with flattened dotted keys and string values
     * @throws IOException if any TOML file cannot be read
     * @throws RuntimeException if TOML parsing fails
     */
    public static Map<String, String> loadToml(String... paths) throws IOException {
        Map<String, String> allBindings = new HashMap<>();
        
        for (String pathStr : paths) {
            Path path = Paths.get(pathStr);
            if (!Files.exists(path)) {
                throw new IOException("TOML file not found: " + path);
            }
            
            String content = Files.readString(path);
            TomlParseResult result = Toml.parse(content);
            
            if (result.hasErrors()) {
                throw new RuntimeException("TOML parsing failed: " + result.errors());
            }
            
            // Flatten the TOML structure
            Map<String, String> flattened = flattenToml(result.toTable(), "");
            allBindings.putAll(flattened);
        }
        
        return allBindings;
    }
    
    /**
     * Write flattened key-value bindings to the seqvar database.
     * 
     * @param bindings Map of key-value pairs to write
     * @param ns Namespace for the bindings (default: "SeqVar")
     * @param src Source identifier (default: "seqweb")
     */
    public static void writeTomlToSeqvar(Map<String, String> bindings, String ns, String src) {
        for (Map.Entry<String, String> entry : bindings.entrySet()) {
            SeqVar.set(ns, entry.getKey(), entry.getValue(), src);
        }
    }
    
    /**
     * Write flattened key-value bindings to the seqvar database with default namespace and source.
     * 
     * @param bindings Map of key-value pairs to write
     */
    public static void writeTomlToSeqvar(Map<String, String> bindings) {
        writeTomlToSeqvar(bindings, "SeqVar", "seqweb");
    }
}
