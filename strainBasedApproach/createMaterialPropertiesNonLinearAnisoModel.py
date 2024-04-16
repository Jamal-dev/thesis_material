import numpy as np
import abaqusConstants as const
import abaqus

createdModel = abaqus.mdb.models['Model-1']

def createMatPropUhyperStrainAnisoModel(Model):
    parent_path = r'D:\Leibniz University\thesis\materialModelTestingMatlab\strainBasedApproach'
    C = np.genfromtxt('%s\CnonLinAnsioProp.csv'%parent_path,delimiter = ',')
    E = np.genfromtxt('%s\EnonLinAnsioProp.csv'%parent_path,delimiter = ',')
    def extractUpperPart(C):
        elements = []
        for i in range(C.shape[0]):
            for j in range(C.shape[1]):
                if j>=i:
                    elements.append(C[i,j])
        return elements
    Clin = extractUpperPart(C)
    Elin = extractUpperPart(E)
    Clin.extend(Elin)
    material_model_name = 'uhyper-strain-subroutine' 
    Model.Material(name=material_model_name)
    Model.materials[material_model_name].Hyperelastic(
        materialType=const.ANISOTROPIC,
        anisotropicType = const.USER_DEFINED,
        properties = 42,
        moduliTimeScale=const.INSTANTANEOUS,
        table = (tuple(Clin),)
    )


createMatPropUhyperStrainAnisoModel(createdModel)

