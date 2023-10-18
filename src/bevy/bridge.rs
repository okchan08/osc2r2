use bevy::prelude::Resource;

use crate::open_drive::OpenDrive;

#[derive(Resource)]
pub struct BevyOpenDriveWrapper {
    pub open_drive: OpenDrive,
}
