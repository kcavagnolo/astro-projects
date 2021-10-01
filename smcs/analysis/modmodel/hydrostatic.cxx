// XSPEC interface to the clmass model

#include "xsTypes.h"
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSFunctions/funcType.h>

#include "Potential.h"


extern "C" void hydrostatic (const EnergyPointer& energyArray,
			     const std::vector<Real>& parameterValues,
			     GroupFluxContainer& flux,
			     GroupFluxContainer& fluxError,
			     MixBase** mixGenerator,
			     const string& modName)
{
  using namespace XSContainer;
  static const string CLMASS ("ModelIndependentMass");
  // mixBase pointers are owned by ModelContainer which is a singleton.
  ModelContainer* m (ModelContainer::Instance ());

  // on first call, set the pointer addressed by mixGenerator to 
  // be returned to the calling routine.
  try {
    if (!*mixGenerator) {
      if (!m->mixingComponents (CLMASS)) {
	m->mixingComponents (CLMASS, new MIClusterMassModel (CLMASS));
      }
      *mixGenerator = m->mixingComponents (CLMASS);
      (*mixGenerator)->initialize (parameterValues);
    }

    (*mixGenerator)->perform (energyArray, parameterValues, flux, fluxError);
  }
  catch (...) {
    // If it throws out of here, MixComponent::perform may never
    // get the chance to set its mixGenerator pointer.  Therefore,
    // can't rely on it to initiate destruction of the faulty
    // MixBase object.  So, do it now.
    m->removeMixingComponent (CLMASS);
    *mixGenerator = 0;
    throw;
  }
}
