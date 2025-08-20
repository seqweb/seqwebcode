// Created by Waldo 2025-08-20

import java.nio.file.*; import java.sql.*; import java.time.Instant;

public final class SeqVar {
  public static final class SeqVarException extends RuntimeException {
    public SeqVarException(String m) { super(m); }
    public SeqVarException(String m, Throwable t) { super(m, t); }
  }

  private static Path dbPath() {
    String home = System.getenv("SEQWEBDEV_HOME");
    if (home == null || home.isEmpty())
      throw new SeqVarException("SEQWEBDEV_HOME is not set");
    Path p = Paths.get(home).toAbsolutePath().resolve(".state").resolve("env.sqlite");
    if (!Files.exists(p))
      throw new SeqVarException("seqvar store not initialized: " + p + " missing. Run SeqWeb bootstrap.");
    return p;
  }

  private static Connection conn() {
    try {
      return DriverManager.getConnection("jdbc:sqlite:" + dbPath().toString());
    } catch (SQLException e) {
      throw new SeqVarException("cannot open seqvar store", e);
    }
  }

  public static String get(String key) { return get("SeqVar", key); }

  public static String get(String ns, String key) {
    try (Connection c = conn();
         PreparedStatement p = c.prepareStatement("SELECT val FROM seqvars WHERE ns=? AND key=?")) {
      p.setString(1, ns); p.setString(2, key);
      try (ResultSet rs = p.executeQuery()) {
        return rs.next() ? (rs.getString(1) == null ? "" : rs.getString(1)) : "";
      }
    } catch (SQLException e) {
      throw new SeqVarException("seqvar table missing. Run SeqWeb bootstrap.", e);
    }
  }

  public static void set(String key, String val) { set("SeqVar", key, val, "seqweb"); }

  public static void set(String ns, String key, String val, String src) {
    long now = Instant.now().getEpochSecond();
    try (Connection c = conn();
         PreparedStatement p = c.prepareStatement(
           "INSERT INTO seqvars(ns,key,val,src,ts) VALUES(?,?,?,?,?) " +
           "ON CONFLICT(ns,key) DO UPDATE SET val=excluded.val, src=excluded.src, ts=excluded.ts")) {
      p.setString(1, ns); p.setString(2, key); p.setString(3, val); p.setString(4, src); p.setLong(5, now);
      p.executeUpdate();
    } catch (SQLException e) {
      throw new SeqVarException("seqvar table missing. Run SeqWeb bootstrap.", e);
    }
  }
}
