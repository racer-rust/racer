use log::{self, LogRecord, LogLevel, LogMetadata};

pub struct SimpleLogger;

impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &LogMetadata) -> bool {
        metadata.level() <= LogLevel::Trace
    }

    fn log(&self, record: &LogRecord) {
        if self.enabled(record.metadata()) &&
            record.location().module_path().starts_with("racer") {
            println!("{}", record.args());
        }
    }
}
