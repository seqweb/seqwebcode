package libs.core;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.PrintStream;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.function.Function;

/**
 * Wrapper utilities for polyglot pipeline modules.
 * 
 * This class provides the Java equivalent of the Python wrapper.py functionality,
 * including typed-value registry for converting between native objects and
 * JSON-serializable formats for cross-language compatibility.
 * 
 * Note: This implementation uses a simple JSON parser to avoid external dependencies.
 * For production use, consider using Jackson or another JSON library.
 */
public class Wrapper {
    
    /**
     * Typed-value registry for polyglot pipeline compatibility.
     * Maps external type IDs to internal type information and conversion functions.
     */
    private static final Map<String, TypeRegistryEntry> TYPED_VALUE_REGISTRY = new HashMap<>();
    
    static {
        // Initialize with basic types - Graph support would need Apache Jena
        // For now, we'll add a placeholder for future Graph support
        // TYPED_VALUE_REGISTRY.put("https://www.w3.org/TR/json-ld11/", 
        //     new TypeRegistryEntry(Graph.class, graph -> serializeGraph(graph), json -> deserializeGraph(json)));
    }
    
    /**
     * Registry entry for type conversion functions.
     */
    private static class TypeRegistryEntry {
        public final Class<?> internalType;
        public final Function<Object, String> toJsonFunction;
        public final Function<String, Object> fromJsonFunction;
        
        public TypeRegistryEntry(Class<?> internalType, 
                               Function<Object, String> toJsonFunction,
                               Function<String, Object> fromJsonFunction) {
            this.internalType = internalType;
            this.toJsonFunction = toJsonFunction;
            this.fromJsonFunction = fromJsonFunction;
        }
    }
    
    /**
     * Argument definition for command-line argument parsing.
     */
    public static class ArgumentDefinition {
        public final String name;
        public final Class<?> type;
        public final String help;
        public final boolean required;
        
        public ArgumentDefinition(String name, Class<?> type, String help, boolean required) {
            this.name = name;
            this.type = type;
            this.help = help;
            this.required = required;
        }
    }
    
    /**
     * Get the external type ID for a given internal type.
     * 
     * @param internalType The internal Java type
     * @return External type ID string, or null if not found
     */
    public static String getExternalTypeId(Class<?> internalType) {
        for (Map.Entry<String, TypeRegistryEntry> entry : TYPED_VALUE_REGISTRY.entrySet()) {
            if (entry.getValue().internalType == internalType) {
                return entry.getKey();
            }
        }
        return null;
    }
    
    /**
     * Get the internal type for a given external type ID.
     * 
     * @param externalTypeId The external type ID string
     * @return Internal Java type, or null if not found
     */
    public static Class<?> getInternalType(String externalTypeId) {
        TypeRegistryEntry entry = TYPED_VALUE_REGISTRY.get(externalTypeId);
        return entry != null ? entry.internalType : null;
    }
    
    /**
     * Get the to-JSON conversion function for a given external type ID.
     * 
     * @param externalTypeId The external type ID string
     * @return Conversion function, or null if not found
     */
    public static Function<Object, String> getToJsonFunction(String externalTypeId) {
        TypeRegistryEntry entry = TYPED_VALUE_REGISTRY.get(externalTypeId);
        return entry != null ? entry.toJsonFunction : null;
    }
    
    /**
     * Get the from-JSON conversion function for a given external type ID.
     * 
     * @param externalTypeId The external type ID string
     * @return Conversion function, or null if not found
     */
    public static Function<String, Object> getFromJsonFunction(String externalTypeId) {
        TypeRegistryEntry entry = TYPED_VALUE_REGISTRY.get(externalTypeId);
        return entry != null ? entry.fromJsonFunction : null;
    }
    
    /**
     * Check if an object is of a registered type.
     * 
     * @param obj Object to check
     * @return true if the object is of a registered type
     */
    public static boolean isRegisteredType(Object obj) {
        return getExternalTypeId(obj.getClass()) != null;
    }
    
    /**
     * Convert an object to JSON-serializable format using the typed-value registry.
     * 
     * @param obj Object to convert
     * @return JSON-serializable object
     */
    public static Object convertToJson(Object obj) {
        if (obj == null) {
            return null;
        }
        
        if (obj instanceof Map) {
            @SuppressWarnings("unchecked")
            Map<String, Object> map = (Map<String, Object>) obj;
            Map<String, Object> result = new HashMap<>();
            for (Map.Entry<String, Object> entry : map.entrySet()) {
                result.put(entry.getKey(), convertToJson(entry.getValue()));
            }
            return result;
        }
        
        if (obj instanceof List) {
            @SuppressWarnings("unchecked")
            List<Object> list = (List<Object>) obj;
            List<Object> result = new ArrayList<>();
            for (Object item : list) {
                result.add(convertToJson(item));
            }
            return result;
        }
        
        if (isRegisteredType(obj)) {
            String externalTypeId = getExternalTypeId(obj.getClass());
            Function<Object, String> toJsonFunction = getToJsonFunction(externalTypeId);
            if (toJsonFunction != null) {
                Map<String, Object> result = new HashMap<>();
                result.put("@type", externalTypeId);
                result.put("@value", toJsonFunction.apply(obj));
                return result;
            }
        }
        
        return obj;
    }
    
    /**
     * Convert a JSON object back to native format using the typed-value registry.
     * 
     * @param jsonObj JSON object to convert
     * @return Native object
     */
    public static Object convertFromJson(Object jsonObj) {
        if (jsonObj == null) {
            return null;
        }
        
        if (jsonObj instanceof Map) {
            @SuppressWarnings("unchecked")
            Map<String, Object> map = (Map<String, Object>) jsonObj;
            
            if (map.containsKey("@type") && map.containsKey("@value")) {
                String externalTypeId = (String) map.get("@type");
                Function<String, Object> fromJsonFunction = getFromJsonFunction(externalTypeId);
                if (fromJsonFunction != null) {
                    try {
                        return fromJsonFunction.apply((String) map.get("@value"));
                    } catch (Exception e) {
                        // If conversion fails, return the original object
                        return jsonObj;
                    }
                }
            }
            
            Map<String, Object> result = new HashMap<>();
            for (Map.Entry<String, Object> entry : map.entrySet()) {
                result.put(entry.getKey(), convertFromJson(entry.getValue()));
            }
            return result;
        }
        
        if (jsonObj instanceof List) {
            @SuppressWarnings("unchecked")
            List<Object> list = (List<Object>) jsonObj;
            List<Object> result = new ArrayList<>();
            for (Object item : list) {
                result.add(convertFromJson(item));
            }
            return result;
        }
        
        return jsonObj;
    }
    
    /**
     * Load JSON from stdin and convert to native objects.
     * 
     * @return Map containing the loaded and converted inbox
     */
    public static Map<String, Object> loadInbox() {
        Map<String, Object> inbox = readStdinJson();
        return (Map<String, Object>) convertFromJson(inbox);
    }
    
    /**
     * Augment an inbox with command-line arguments using destructuring merge.
     * 
     * @param inbox Base inbox dictionary to augment
     * @param argumentDefinitions List of argument definitions
     * @return Map containing the merged inbox with CLI arguments
     */
    public static Map<String, Object> augmentInboxWithArgs(Map<String, Object> inbox, 
                                                          List<ArgumentDefinition> argumentDefinitions) {
        if (argumentDefinitions == null) {
            argumentDefinitions = new ArrayList<>();
        }
        
        Map<String, Object> cliArgs = parseCommandLineArgs(argumentDefinitions);
        
        // Merge CLI args into inbox using destructuring (CLI overrides inbox)
        Map<String, Object> result = new HashMap<>(inbox);
        result.putAll(cliArgs);
        return result;
    }
    
    /**
     * Get a complete inbox by loading from stdin and augmenting with CLI arguments.
     * 
     * @param argumentDefinitions List of argument definitions
     * @return Map containing the complete inbox with stdin data and CLI arguments
     */
    public static Map<String, Object> getInbox(List<ArgumentDefinition> argumentDefinitions) {
        Map<String, Object> inbox = loadInbox();
        return augmentInboxWithArgs(inbox, argumentDefinitions);
    }
    
    /**
     * Convert an outbox to dumpable JSON and write it to stdout.
     * 
     * @param outbox Output box from core function (may contain native objects)
     */
    public static void dumpOutbox(Map<String, Object> outbox) {
        Object jsonOutbox = convertToJson(outbox);
        System.out.println(toJson((Map<String, Object>) jsonOutbox));
    }
    
    /**
     * Build an inbox from stdin (JSON) and command-line arguments.
     * 
     * This function implements the unified wrapper contract:
     * 1. Read JSON from stdin (defaults to empty map if empty)
     * 2. Parse command-line arguments according to definitions
     * 3. Merge CLI args into inbox (CLI overrides stdin)
     * 4. Return the merged inbox
     * 
     * @param argumentDefinitions List of argument definitions
     * @return Map containing the merged inbox
     */
    public static Map<String, Object> buildInboxFromArgs(List<ArgumentDefinition> argumentDefinitions) {
        return getInbox(argumentDefinitions);
    }
    
    /**
     * Read JSON from stdin in a non-blocking manner.
     * 
     * @return Map containing parsed JSON or empty map if no input
     */
    private static Map<String, Object> readStdinJson() {
        try {
            // Check if stdin has data available (non-blocking)
            if (System.in.available() > 0) {
                BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
                StringBuilder input = new StringBuilder();
                String line;
                
                // Read all available input
                while ((line = reader.readLine()) != null) {
                    input.append(line).append("\n");
                }
                
                String stdinInput = input.toString().trim();
                if (!stdinInput.isEmpty()) {
                    try {
                        // Parse JSON input using simple parser
                        return parseSimpleJson(stdinInput);
                    } catch (Exception e) {
                        // If stdin is not valid JSON, treat as empty
                        return new HashMap<>();
                    }
                }
            }
        } catch (IOException e) {
            // If there's an error reading stdin, treat as empty
        }
        
        return new HashMap<>();
    }
    
    /**
     * Parse command-line arguments according to the argument definitions.
     * 
     * @param argumentDefinitions List of argument definitions
     * @return Map containing parsed CLI arguments
     */
    private static Map<String, Object> parseCommandLineArgs(List<ArgumentDefinition> argumentDefinitions) {
        Map<String, Object> cliArgs = new HashMap<>();
        
        // Get command-line arguments
        String[] args = System.getProperty("sun.java.command", "").split("\\s+");
        
        // Simple argument parsing (this is a basic implementation)
        // In a production system, you might want to use a proper argument parsing library
        for (int i = 0; i < args.length; i++) {
            String arg = args[i];
            if (arg.startsWith("--")) {
                String argName = arg.substring(2);
                
                // Find matching argument definition
                for (ArgumentDefinition def : argumentDefinitions) {
                    if (def.name.equals(argName)) {
                        Object value = null;
                        
                        if (def.type == Boolean.class || def.type == boolean.class) {
                            // Boolean flag - no value needed
                            value = true;
                        } else if (i + 1 < args.length) {
                            // Get the next argument as the value
                            String argValue = args[++i];
                            value = convertValue(argValue, def.type);
                        }
                        
                        if (value != null) {
                            cliArgs.put(argName, value);
                        }
                        break;
                    }
                }
            }
        }
        
        return cliArgs;
    }
    
    /**
     * Convert string value to the specified type.
     * 
     * @param value String value to convert
     * @param targetType Target type to convert to
     * @return Converted value or null if conversion fails
     */
    private static Object convertValue(String value, Class<?> targetType) {
        try {
            if (targetType == String.class) {
                return value;
            } else if (targetType == Integer.class || targetType == int.class) {
                return Integer.parseInt(value);
            } else if (targetType == Long.class || targetType == long.class) {
                return Long.parseLong(value);
            } else if (targetType == Double.class || targetType == double.class) {
                return Double.parseDouble(value);
            } else if (targetType == Float.class || targetType == float.class) {
                return Float.parseFloat(value);
            } else if (targetType == Boolean.class || targetType == boolean.class) {
                return Boolean.parseBoolean(value);
            }
        } catch (NumberFormatException e) {
            // Conversion failed
        }
        
        return null;
    }
    
    /**
     * Simple JSON parser for basic key-value objects.
     * This is a minimal implementation for the polyglot pipeline use case.
     * 
     * @param json JSON string to parse
     * @return Map containing parsed key-value pairs
     */
    private static Map<String, Object> parseSimpleJson(String json) {
        Map<String, Object> result = new HashMap<>();
        
        // Remove outer braces and whitespace
        json = json.trim();
        if (json.startsWith("{") && json.endsWith("}")) {
            json = json.substring(1, json.length() - 1).trim();
        }
        
        // Simple regex to match key-value pairs
        Pattern pattern = Pattern.compile("\"([^\"]+)\"\\s*:\\s*(\"[^\"]*\"|\\d+(?:\\.\\d+)?|true|false|null)");
        Matcher matcher = pattern.matcher(json);
        
        while (matcher.find()) {
            String key = matcher.group(1);
            String value = matcher.group(2);
            
            // Parse value based on type
            Object parsedValue;
            if (value.startsWith("\"") && value.endsWith("\"")) {
                // String value
                parsedValue = value.substring(1, value.length() - 1);
            } else if ("true".equals(value)) {
                parsedValue = true;
            } else if ("false".equals(value)) {
                parsedValue = false;
            } else if ("null".equals(value)) {
                parsedValue = null;
            } else if (value.contains(".")) {
                // Double value
                try {
                    parsedValue = Double.parseDouble(value);
                } catch (NumberFormatException e) {
                    parsedValue = value; // fallback to string
                }
            } else {
                // Integer value
                try {
                    parsedValue = Integer.parseInt(value);
                } catch (NumberFormatException e) {
                    parsedValue = value; // fallback to string
                }
            }
            
            result.put(key, parsedValue);
        }
        
        return result;
    }
    
    /**
     * Convert a Map to JSON string for output.
     * This is a simple implementation for basic key-value objects.
     * 
     * @param map Map to convert
     * @return JSON string representation
     */
    public static String toJson(Map<String, Object> map) {
        if (map == null || map.isEmpty()) {
            return "{}";
        }
        
        StringBuilder json = new StringBuilder();
        json.append("{");
        
        boolean first = true;
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            if (!first) {
                json.append(",");
            }
            first = false;
            
            json.append("\"").append(entry.getKey()).append("\":");
            
            Object value = entry.getValue();
            if (value == null) {
                json.append("null");
            } else if (value instanceof String) {
                json.append("\"").append(value).append("\"");
            } else if (value instanceof Boolean || value instanceof Number) {
                json.append(value);
            } else {
                // Convert other types to string
                json.append("\"").append(value.toString()).append("\"");
            }
        }
        
        json.append("}");
        return json.toString();
    }
}